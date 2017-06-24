package org.rebeam.tree

import io.circe._
import org.rebeam.lenses._
import cats.syntax.either._
import org.rebeam.tree.ref.{Cache, DeltaCache}
import org.rebeam.tree.sync.Sync._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

object DeltaCodecs {

  case class RefUpdateResult[A](data: A, outgoingRefs: Set[Guid[_]])

  object RefUpdateResult {
    def noRefs[A](a: A): RefUpdateResult[A] = RefUpdateResult(a, Set.empty)
  }

  trait DeltaCodec[A] extends Decoder[Delta[A]] {
    def decoder: Decoder[Delta[A]]

    def updateRefs(a: RefUpdateResult[A], cache: DeltaCache): RefUpdateResult[A]

    def updateRefsDataOnly(a: A, cache: DeltaCache): A = updateRefs(RefUpdateResult.noRefs(a), cache).data

    final def or(d: => DeltaCodec[A]): DeltaCodec[A] = new DeltaCodecOr(this, d)
  }

  class DeltaCodecOr[A](val deltaCodec1: DeltaCodec[A], val deltaCodec2: DeltaCodec[A]) extends DeltaCodec[A] {
    val decoder: Decoder[Delta[A]] = deltaCodec1.decoder or deltaCodec2.decoder
    def updateRefs(a: RefUpdateResult[A], cache: DeltaCache): RefUpdateResult[A] = deltaCodec2.updateRefs(deltaCodec1.updateRefs(a, cache), cache)
    def apply(c: HCursor): Decoder.Result[Delta[A]] = decoder(c)
  }

  class DeltaCodecNoRefs[M](val decoder: Decoder[Delta[M]]) extends DeltaCodec[M] {
    def updateRefs(rur: RefUpdateResult[M], cache: DeltaCache): RefUpdateResult[M] = rur
    def apply(c: HCursor): Decoder.Result[Delta[M]] = decoder(c)
  }

  def value[M: Decoder]: DeltaCodec[M] = new DeltaCodecNoRefs(
    Decoder.instance(c =>
      //Expect an object with a value field, containing an encoded M instance. We
      //map this to a ValueDelta using that instance (i.e. this provides a delta
      //replacing the old instance with the decoded instance)
      c.downField("value").as[M].map(ValueDelta(_))
    )
  )

  class DeltaCodecValueRef[M]() extends DeltaCodec[Ref[M]] {
    def decoder: Decoder[Delta[Ref[M]]] = Decoder.instance(c =>
      c.downField("value").as[Ref[M]].map(ValueDelta(_))
    )
    def updateRefs(rur: RefUpdateResult[Ref[M]], cache: DeltaCache): RefUpdateResult[Ref[M]] =
      RefUpdateResult(cache.updateRef(rur.data).getOrElse(rur.data), rur.outgoingRefs + rur.data.guid)

    def apply(c: HCursor): Decoder.Result[Delta[Ref[M]]] = decoder(c)
  }

  implicit def refValueDeltaCodec[M] = new DeltaCodecValueRef[M]

  class DeltaCodecLensN[M, A](namedLens: LensN[M, A])(implicit dCodecA: DeltaCodec[A]) extends DeltaCodec[M] {

    val decoder: Decoder[Delta[M]] = Decoder.instance(c =>
      //This decodes a LensDelta on a data item of type M, using a lens from M to B.
      //The top level object has a "lens" field as a type identifier, with value encoding the delta.
      //We then require an object with a field matching th e lens' name, if we find this we
      //decode the value of that field as a Delta[B]. Finally, we wrap this
      //Delta[B] in a LensNDelta[M, B] to make it into a Delta[M].
      c.downField("lens").downField("lensN").downField(namedLens.name).as[Delta[A]].map(LensNDelta(namedLens, _))
    )

    // Use dCodecA to handle data reached using the lens
    def updateRefs(rur: RefUpdateResult[M], cache: DeltaCache): RefUpdateResult[M] = {
      //TODO any need to check for whether values have changed?
      val field = namedLens.get(rur.data)
      val fieldRUR = dCodecA.updateRefs(rur.copy(data = field), cache)
      val newData = namedLens.set(fieldRUR.data)(rur.data)
      RefUpdateResult(newData, fieldRUR.outgoingRefs)
//      namedLens.modify(a => dCodecA.updateRefs(a, cache))(rur.data)
    }

    def apply(c: HCursor): Decoder.Result[Delta[M]] = decoder(c)
  }

  def lensN[M, A](namedLens: LensN[M, A])(implicit dCodecA: DeltaCodec[A]): DeltaCodec[M] = new DeltaCodecLensN[M, A](namedLens)(dCodecA)

  private def listUpdateRefs[A](m: RefUpdateResult[List[A]], cache: DeltaCache)(implicit dCodecA: DeltaCodec[A]): RefUpdateResult[List[A]] = {
    // Hidden mutability
    val newList = new ListBuffer[A]
    val newRefs = new mutable.HashSet[Guid[_]]()
    newRefs ++= m.outgoingRefs
    for (a <- m.data) {
      val rur = RefUpdateResult.noRefs(a)
      val rur2 = dCodecA.updateRefs(rur, cache)
      newList += rur2.data
      newRefs ++= rur2.outgoingRefs
    }
    RefUpdateResult(newList.toList, newRefs.toSet)
    //      m.map(a => dCodecA.updateRefs(a, cache))
  }

  class DeltaCodecOptionalI[A](implicit dCodecA: DeltaCodec[A]) extends DeltaCodec[List[A]] {
    def decoder: Decoder[Delta[List[A]]] = Decoder.instance(c => {
      val o = c.downField("optional").downField("optionalI")
      for {
        index <- o.downField("index").as[Int]
        delta <- o.downField("delta").as[Delta[A]]
      } yield OptionalIDelta[A](OptionalI(index), delta)
    })

    def updateRefs(m: RefUpdateResult[List[A]], cache: DeltaCache): RefUpdateResult[List[A]] = listUpdateRefs(m, cache)

    def apply(c: HCursor): Decoder.Result[Delta[List[A]]] = decoder(c)
  }

  def optionalI[A](implicit dCodecA: DeltaCodec[A]): DeltaCodec[List[A]] = new DeltaCodecOptionalI[A]

  class DeltaCodecOptionalMatch[A, F <: A => Boolean](implicit dCodecA: DeltaCodec[A], cDecoder: Decoder[F]) extends DeltaCodec[List[A]] {

    def decoder: Decoder[Delta[List[A]]] = Decoder.instance(c => {
      val o = c.downField("optional").downField("optionalMatch")
      for {
        f <- o.downField("find").as[F]
        delta <- o.downField("delta").as[Delta[A]]
      } yield OptionalMatchDelta[A, F](OptionalMatch(f), delta)
    })

    def updateRefs(m: RefUpdateResult[List[A]], cache: DeltaCache): RefUpdateResult[List[A]] = listUpdateRefs(m, cache)

    def apply(c: HCursor): Decoder.Result[Delta[List[A]]] = decoder(c)
  }

  def optionalMatch[A, F <: A => Boolean](implicit dCodecA: DeltaCodec[A], cDecoder: Decoder[F]): DeltaCodec[List[A]] =
    new DeltaCodecOptionalMatch[A, F]

  class DeltaCodecOption[A](implicit dCodecA: DeltaCodec[A]) extends DeltaCodec[Option[A]] {

    val decoder: Decoder[Delta[Option[A]]] = Decoder.instance(c => {
      val o = c.downField("option")
      for {
        delta <- o.downField("delta").as[Delta[A]]
      } yield OptionDelta[A](delta)
    })

    def updateRefs(m: RefUpdateResult[Option[A]], cache: DeltaCache): RefUpdateResult[Option[A]] = m.data match {
      case None => m
      case Some(a) => {
        //FIXME can we really change parametric type with copy?
        val rur = dCodecA.updateRefs(m.copy(data = a), cache)
        rur.copy(data = Some(rur.data))
      }
    }

    def apply(c: HCursor): Decoder.Result[Delta[Option[A]]] = decoder(c)
  }

  def option[A](implicit dCodecA: DeltaCodec[A]): DeltaCodec[Option[A]] = new DeltaCodecOption[A]

  class DeltaCodecPrismN[S, A](prismN: PrismN[S, A])(implicit dCodecA: DeltaCodec[A]) extends DeltaCodec[S] {

    val decoder: Decoder[Delta[S]] = Decoder.instance(c => {
      c.downField("prism").downField("prismN").downField(prismN.name).as[Delta[A]].map(delta => PrismNDelta[S, A](prismN, delta))
    })

    // Use dCodecA to handle data reached using the prism
    def updateRefs(rur: RefUpdateResult[S], cache: DeltaCache): RefUpdateResult[S] = {
      //TODO any need to check for whether values have changed?
      val fieldO = prismN.getOption(rur.data)
      fieldO match {
        case None => rur
        case Some(field) =>
          val fieldRUR = dCodecA.updateRefs(rur.copy(data = field), cache)
          val newData = prismN.set(fieldRUR.data)(rur.data)
          RefUpdateResult(newData, fieldRUR.outgoingRefs)
      }
    }

    def apply(c: HCursor): Decoder.Result[Delta[S]] = decoder(c)
  }

  def prismN[S, A](prismN: PrismN[S, A])(implicit dCodecA: DeltaCodec[A]): DeltaCodec[S] =
    new DeltaCodecPrismN(prismN)


  def action[M, A <: Delta[M] : Decoder]: DeltaCodec[M] = new DeltaCodecNoRefs(
    Decoder.instance(c =>
      //Expect an object with a value field, containing an encoded M instance. We
      //map this to a ValueDelta using that instance (i.e. this provides a delta
      //replacing the old instance with the decoded instance)
      c.downField("action").as[A]

    //Map to Delta[M] for neatness
    ).map(a => a: Delta[M])
  )

  class DeltaCodecCache[M, A <: M](implicit dCodecA: DeltaCodec[A]) extends DeltaCodec[Cache[M]] {

    //FIXME this is unsound - it will decode to eagerly. Since the type A is not encoded
    //in the delta Json (and in particular not in the Json for the Ref[A]), we are happy
    //to decode anything where the delta Json matches an expected delta for the type A.
    //So for example say we have both Cats and Dogs in a Cache[Animal],
    val decoder: Decoder[Delta[Cache[M]]] = Decoder.instance(c => {
      val o = c.downField("optional").downField("optionalCache")
      for {
        ref <- o.downField("ref").as[Ref[A]]
        delta <- o.downField("delta").as[Delta[A]]
      } yield CacheDelta[M, A](OptionalCache(ref), delta)
    })

    def updateRefs(rur: RefUpdateResult[Cache[M]], cache: DeltaCache): RefUpdateResult[Cache[M]] = rur
    def apply(c: HCursor): Decoder.Result[Delta[Cache[M]]] = decoder(c)
  }

}
