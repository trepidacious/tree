package org.rebeam.tree

import io.circe._
import org.rebeam.lenses._
import cats.syntax.either._
import org.rebeam.tree.ref.{Cache, RefUpdater}
import org.rebeam.tree.sync.Sync._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import org.rebeam.tree.ref.Ref

object DeltaCodecs {

  /**
    * DeltaCodec for Unit - decodes anything to the identity delta since
    * we can't meaningfully edit Unit, and does nothing with refs since
    * Unit has none
    */
//  val unitDeltaCodec: DeltaCodec[Unit] = new DeltaCodecNoRefs(
//    Decoder.instance(_ => Either.right(Delta.identity[Unit]))
//  )

  case class RefUpdateResult[A](data: A, outgoingRefs: Set[Guid[_]])

  object RefUpdateResult {
    def noRefs[A](a: A): RefUpdateResult[A] = RefUpdateResult(a, Set.empty)
  }

  trait DeltaCodec[C, A] extends Decoder[Delta[C, A]] {
    def decoder: Decoder[Delta[C, A]]

    def updateRefs(a: RefUpdateResult[A], updater: RefUpdater): RefUpdateResult[A]

    def updateRefsDataOnly(a: A, updater: RefUpdater): A = updateRefs(RefUpdateResult.noRefs(a), updater).data

    final def or(d: => DeltaCodec[C, A]): DeltaCodec[C, A] = new DeltaCodecOr(this, d)
  }

  class DeltaCodecOr[C, A](val deltaCodec1: DeltaCodec[C, A], val deltaCodec2: DeltaCodec[C, A]) extends DeltaCodec[C, A] {
    val decoder: Decoder[Delta[C, A]] = deltaCodec1.decoder or deltaCodec2.decoder
    def updateRefs(a: RefUpdateResult[A], updater: RefUpdater): RefUpdateResult[A] = deltaCodec2.updateRefs(deltaCodec1.updateRefs(a, updater), updater)
    def apply(c: HCursor): Decoder.Result[Delta[C, A]] = decoder(c)
  }

  class DeltaCodecNoRefs[C, M](val decoder: Decoder[Delta[C, M]]) extends DeltaCodec[C, M] {
    def updateRefs(rur: RefUpdateResult[M], updater: RefUpdater): RefUpdateResult[M] = rur
    def apply(c: HCursor): Decoder.Result[Delta[C, M]] = decoder(c)
  }

  def value[C, M: Decoder]: DeltaCodec[C, M] = new DeltaCodecNoRefs(
    Decoder.instance(c =>
      //Expect an object with a value field, containing an encoded M instance. We
      //map this to a ValueDelta using that instance (i.e. this provides a delta
      //replacing the old instance with the decoded instance)
      c.downField("value").as[M].map(ValueDelta[C, M](_))
    )
  )

  class DeltaCodecValueRef[C, M]() extends DeltaCodec[C, Ref[M]] {
    def decoder: Decoder[Delta[C, Ref[M]]] = Decoder.instance(c =>
      c.downField("value").as[Ref[M]].map(ValueDelta(_))
    )
    def updateRefs(rur: RefUpdateResult[Ref[M]], updater: RefUpdater): RefUpdateResult[Ref[M]] =
      RefUpdateResult(updater.updateRef(rur.data).getOrElse(rur.data), rur.outgoingRefs + rur.data.guid)

    def apply(c: HCursor): Decoder.Result[Delta[C, Ref[M]]] = decoder(c)
  }

  implicit def refValueDeltaCodec[C, M] = new DeltaCodecValueRef[C, M]

  class DeltaCodecLensN[C, M, A](namedLens: LensN[M, A])(implicit dCodecA: DeltaCodec[C, A]) extends DeltaCodec[C, M] {

    val decoder: Decoder[Delta[C, M]] = Decoder.instance(c =>
      //This decodes a LensDelta on a data item of type M, using a lens from M to B.
      //The top level object has a "lens" field as a type identifier, with value encoding the delta.
      //We then require an object with a field matching th e lens' name, if we find this we
      //decode the value of that field as a Delta[B]. Finally, we wrap this
      //Delta[B] in a LensNDelta[M, B] to make it into a Delta[M].
      c.downField("lens").downField("lensN").downField(namedLens.name).as[Delta[C, A]].map(LensNDelta(namedLens, _))
    )

    // Use dCodecA to handle data reached using the lens
    def updateRefs(rur: RefUpdateResult[M], updater: RefUpdater): RefUpdateResult[M] = {
      //TODO any need to check for whether values have changed?
      val field = namedLens.get(rur.data)
      val fieldRUR = dCodecA.updateRefs(rur.copy(data = field), updater)
      val newData = namedLens.set(fieldRUR.data)(rur.data)
      RefUpdateResult(newData, fieldRUR.outgoingRefs)
//      namedLens.modify(a => dCodecA.updateRefs(a, updater))(rur.data)
    }

    def apply(c: HCursor): Decoder.Result[Delta[C, M]] = decoder(c)
  }

  def lensN[C, M, A](namedLens: LensN[M, A])(implicit dCodecA: DeltaCodec[C, A]): DeltaCodec[C, M] = new DeltaCodecLensN[C, M, A](namedLens)(dCodecA)

  private def listUpdateRefs[C, A](m: RefUpdateResult[List[A]], updater: RefUpdater)(implicit dCodecA: DeltaCodec[C, A]): RefUpdateResult[List[A]] = {
    // Hidden mutability
    val newList = new ListBuffer[A]
    val newRefs = new mutable.HashSet[Guid[_]]()
    newRefs ++= m.outgoingRefs
    for (a <- m.data) {
      val rur = RefUpdateResult.noRefs(a)
      val rur2 = dCodecA.updateRefs(rur, updater)
      newList += rur2.data
      newRefs ++= rur2.outgoingRefs
    }
    RefUpdateResult(newList.toList, newRefs.toSet)
    //      m.map(a => dCodecA.updateRefs(a, updater))
  }

  class DeltaCodecOptionalI[C, A](implicit dCodecA: DeltaCodec[C, A]) extends DeltaCodec[C, List[A]] {
    def decoder: Decoder[Delta[C, List[A]]] = Decoder.instance(c => {
      val o = c.downField("optional").downField("optionalI")
      for {
        index <- o.downField("index").as[Int]
        delta <- o.downField("delta").as[Delta[C, A]]
      } yield OptionalIDelta[C, A](OptionalI(index), delta)
    })

    def updateRefs(m: RefUpdateResult[List[A]], updater: RefUpdater): RefUpdateResult[List[A]] = listUpdateRefs(m, updater)

    def apply(c: HCursor): Decoder.Result[Delta[C, List[A]]] = decoder(c)
  }

  def optionalI[C, A](implicit dCodecA: DeltaCodec[C, A]): DeltaCodec[C, List[A]] = new DeltaCodecOptionalI[C, A]

  class DeltaCodecOptionalMatch[C, A, F <: A => Boolean](implicit dCodecA: DeltaCodec[C, A], cDecoder: Decoder[F]) extends DeltaCodec[C, List[A]] {

    def decoder: Decoder[Delta[C, List[A]]] = Decoder.instance(c => {
      val o = c.downField("optional").downField("optionalMatch")
      for {
        f <- o.downField("find").as[F]
        delta <- o.downField("delta").as[Delta[C, A]]
      } yield OptionalMatchDelta[C, A, F](OptionalMatch(f), delta)
    })

    def updateRefs(m: RefUpdateResult[List[A]], updater: RefUpdater): RefUpdateResult[List[A]] = listUpdateRefs(m, updater)

    def apply(c: HCursor): Decoder.Result[Delta[C, List[A]]] = decoder(c)
  }

  def optionalMatch[C, A, F <: A => Boolean](implicit dCodecA: DeltaCodec[C, A], cDecoder: Decoder[F]): DeltaCodec[C, List[A]] =
    new DeltaCodecOptionalMatch[C, A, F]

  class DeltaCodecOption[C, A](implicit dCodecA: DeltaCodec[C, A]) extends DeltaCodec[C, Option[A]] {

    val decoder: Decoder[Delta[C, Option[A]]] = Decoder.instance(c => {
      val o = c.downField("option")
      for {
        delta <- o.downField("delta").as[Delta[C, A]]
      } yield OptionDelta[C, A](delta)
    })

    def updateRefs(m: RefUpdateResult[Option[A]], updater: RefUpdater): RefUpdateResult[Option[A]] = m.data match {
      case None => m
      case Some(a) => {
        //FIXME can we really change parametric type with copy?
        val rur = dCodecA.updateRefs(m.copy(data = a), updater)
        rur.copy(data = Some(rur.data))
      }
    }

    def apply(c: HCursor): Decoder.Result[Delta[C, Option[A]]] = decoder(c)
  }

  def option[C, A](implicit dCodecA: DeltaCodec[C, A]): DeltaCodec[C, Option[A]] = new DeltaCodecOption[C, A]

  class DeltaCodecPrismN[C, S, A](prismN: PrismN[S, A])(implicit dCodecA: DeltaCodec[C, A]) extends DeltaCodec[C, S] {

    val decoder: Decoder[Delta[C, S]] = Decoder.instance(c => {
      c.downField("prism").downField("prismN").downField(prismN.name).as[Delta[C, A]].map(delta => PrismNDelta[C, S, A](prismN, delta))
    })

    // Use dCodecA to handle data reached using the prism
    def updateRefs(rur: RefUpdateResult[S], updater: RefUpdater): RefUpdateResult[S] = {
      //TODO any need to check for whether values have changed?
      val fieldO = prismN.getOption(rur.data)
      fieldO match {
        case None => rur
        case Some(field) =>
          val fieldRUR = dCodecA.updateRefs(rur.copy(data = field), updater)
          val newData = prismN.set(fieldRUR.data)(rur.data)
          RefUpdateResult(newData, fieldRUR.outgoingRefs)
      }
    }

    def apply(c: HCursor): Decoder.Result[Delta[C, S]] = decoder(c)
  }

  def prismN[C, S, A](prismN: PrismN[S, A])(implicit dCodecA: DeltaCodec[C, A]): DeltaCodec[C, S] =
    new DeltaCodecPrismN(prismN)


  def action[C, M, A <: Delta[C, M] : Decoder]: DeltaCodec[C, M] = new DeltaCodecNoRefs(
    Decoder.instance(c =>
      //Expect an object with a value field, containing an encoded M instance. We
      //map this to a ValueDelta using that instance (i.e. this provides a delta
      //replacing the old instance with the decoded instance)
      c.downField("action").as[A]

    //Map to Delta[M] for neatness
    ).map(a => a: Delta[C, M])
  )

  class DeltaCodecCache[C, A](implicit dCodecA: DeltaCodec[C, A]) extends DeltaCodec[C, Cache[A]] {

    val decoder: Decoder[Delta[C, Cache[A]]] = Decoder.instance(c => {
      val o = c.downField("optional").downField("optionalCache")
      for {
        ref <- o.downField("ref").as[Ref[A]]
        delta <- o.downField("delta").as[Delta[C, A]]
      } yield CacheDelta[C, A](OptionalCache(ref), delta)
    })

    def updateRefs(rur: RefUpdateResult[Cache[A]], updater: RefUpdater): RefUpdateResult[Cache[A]] = rur
    def apply(c: HCursor): Decoder.Result[Delta[C, Cache[A]]] = decoder(c)
  }

  implicit def cache[C, A](implicit dCodecA: DeltaCodec[C, A]): DeltaCodec[C, Cache[A]] = new DeltaCodecCache[C, A]

}
