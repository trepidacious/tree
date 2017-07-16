package org.rebeam.tree

import io.circe._
import org.rebeam.lenses._
import cats.syntax.either._
import org.rebeam.tree.ref.{Mirror, MirrorCodec}
import org.rebeam.tree.ref.RefUpdater
import org.rebeam.tree.sync.Sync._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.higherKinds
import org.rebeam.tree.ref.Ref

object DeltaCodecs {

  case class RefUpdateResult[A](data: A, outgoingRefs: Set[Guid[_]])

  object RefUpdateResult {
    def noRefs[A](a: A): RefUpdateResult[A] = RefUpdateResult(a, Set.empty)
  }

  trait DeltaCodec[A] extends Decoder[Delta[A]] {
    def decoder: Decoder[Delta[A]]

    def updateRefs(a: RefUpdateResult[A], updater: RefUpdater): RefUpdateResult[A]

    def updateRefsDataOnly(a: A, updater: RefUpdater): A = updateRefs(RefUpdateResult.noRefs(a), updater).data

    final def or(d: => DeltaCodec[A]): DeltaCodec[A] = new DeltaCodecOr(this, d)
  }

  class DeltaCodecOr[A](val deltaCodec1: DeltaCodec[A], val deltaCodec2: DeltaCodec[A]) extends DeltaCodec[A] {
    val decoder: Decoder[Delta[A]] = deltaCodec1.decoder or deltaCodec2.decoder
    def updateRefs(a: RefUpdateResult[A], updater: RefUpdater): RefUpdateResult[A] = deltaCodec2.updateRefs(deltaCodec1.updateRefs(a, updater), updater)
    def apply(c: HCursor): Decoder.Result[Delta[A]] = decoder(c)
  }

  class DeltaCodecNoRefs[M](val decoder: Decoder[Delta[M]]) extends DeltaCodec[M] {
    def updateRefs(rur: RefUpdateResult[M], updater: RefUpdater): RefUpdateResult[M] = rur
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
    def updateRefs(rur: RefUpdateResult[Ref[M]], updater: RefUpdater): RefUpdateResult[Ref[M]] =
      RefUpdateResult(updater.updateRef(rur.data).getOrElse(rur.data), rur.outgoingRefs + rur.data.guid)

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
    def updateRefs(rur: RefUpdateResult[M], updater: RefUpdater): RefUpdateResult[M] = {
      //TODO any need to check for whether values have changed?
      val field = namedLens.get(rur.data)
      val fieldRUR = dCodecA.updateRefs(rur.copy(data = field), updater)
      val newData = namedLens.set(fieldRUR.data)(rur.data)
      RefUpdateResult(newData, fieldRUR.outgoingRefs)
//      namedLens.modify(a => dCodecA.updateRefs(a, updater))(rur.data)
    }

    def apply(c: HCursor): Decoder.Result[Delta[M]] = decoder(c)
  }

  def lensN[M, A](namedLens: LensN[M, A])(implicit dCodecA: DeltaCodec[A]): DeltaCodec[M] = new DeltaCodecLensN[M, A](namedLens)(dCodecA)

  private def listUpdateRefs[A](m: RefUpdateResult[List[A]], updater: RefUpdater)(implicit dCodecA: DeltaCodec[A]): RefUpdateResult[List[A]] = {
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

  class DeltaCodecOptionalI[A](implicit dCodecA: DeltaCodec[A]) extends DeltaCodec[List[A]] {
    def decoder: Decoder[Delta[List[A]]] = Decoder.instance(c => {
      val o = c.downField("optional").downField("optionalI")
      for {
        index <- o.downField("index").as[Int]
        delta <- o.downField("delta").as[Delta[A]]
      } yield OptionalIDelta[A](OptionalI(index), delta)
    })

    def updateRefs(m: RefUpdateResult[List[A]], updater: RefUpdater): RefUpdateResult[List[A]] = listUpdateRefs(m, updater)

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

    def updateRefs(m: RefUpdateResult[List[A]], updater: RefUpdater): RefUpdateResult[List[A]] = listUpdateRefs(m, updater)

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

    def updateRefs(m: RefUpdateResult[Option[A]], updater: RefUpdater): RefUpdateResult[Option[A]] = m.data match {
      case None => m
      case Some(a) => {
        //FIXME can we really change parametric type with copy?
        val rur = dCodecA.updateRefs(m.copy(data = a), updater)
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

  class DeltaCodecMirror[A](implicit mirrorCodecA: MirrorCodec[A]) extends DeltaCodec[Mirror] {

    val decoder: Decoder[Delta[Mirror]] = Decoder.instance(c => {
      // Note we require the json to contain the mirror codec's type name
      val o = c.downField("mirror").downField(mirrorCodecA.mirrorType.name)
      for {
        ref <- o.downField("ref").as[Ref[A]]
        delta <- o.downField("delta").as[Delta[A]]
      } yield MirrorDelta[A](ref, delta)
    })

    def updateRefs(rur: RefUpdateResult[Mirror], updater: RefUpdater): RefUpdateResult[Mirror] = rur
    def apply(c: HCursor): Decoder.Result[Delta[Mirror]] = decoder(c)
  }

  // Not implicit - this is used from Mirror codec builder to make the delta codec to go with plain
  // Codec[Mirror]
  def mirror[A](implicit mirrorCodecA: MirrorCodec[A]): DeltaCodec[Mirror] = new DeltaCodecMirror[A]

}
