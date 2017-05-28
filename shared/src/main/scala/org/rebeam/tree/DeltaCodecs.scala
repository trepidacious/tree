package org.rebeam.tree

import io.circe._
import org.rebeam.lenses._
import cats.syntax.either._
import org.rebeam.tree.sync.Sync
import org.rebeam.tree.sync.Sync.Ref.RefResolved
import org.rebeam.tree.sync.Sync._

import scala.language.higherKinds

object DeltaCodecs {

  trait Cache {

    def apply[A](ref: Ref[A]): Option[A]

    /**
      * Update a ref to the latest version if necessary
      * @param ref  The ref to update
      * @tparam A   The type of the referent
      * @return     Some(updated ref) if update is needed, None otherwise
      */
    def updateRef[A](ref: Ref[A]): Option[Ref[A]]
  }

  class DeltaCodecOr[A](val deltaCodec1: DeltaCodec[A], val deltaCodec2: DeltaCodec[A]) extends DeltaCodec[A] {
    val decoder: Decoder[Delta[A]] = deltaCodec1.decoder or deltaCodec2.decoder
    def updateRefs(a: A, cache: Cache): A = deltaCodec2.updateRefs(deltaCodec1.updateRefs(a, cache), cache)
    def apply(c: HCursor): Decoder.Result[Delta[A]] = decoder(c)
  }

  trait DeltaCodec[A] extends Decoder[Delta[A]] {
    def decoder: Decoder[Delta[A]]

    def updateRefs(a: A, cache: Cache): A

    final def or(d: => DeltaCodec[A]): DeltaCodec[A] = new DeltaCodecOr(this, d)
  }

  class DeltaCodecNoRefs[M](val decoder: Decoder[Delta[M]]) extends DeltaCodec[M] {
    def updateRefs(m: M, cache: Cache): M = m
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
    def updateRefs(m: Ref[M], cache: Cache): Ref[M] = cache.updateRef(m).getOrElse(m)
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
    def updateRefs(m: M, cache: Cache): M = namedLens.modify(a => dCodecA.updateRefs(a, cache))(m)

    def apply(c: HCursor): Decoder.Result[Delta[M]] = decoder(c)
  }

  def lensN[M, A](namedLens: LensN[M, A])(implicit dCodecA: DeltaCodec[A]): DeltaCodec[M] = new DeltaCodecLensN[M, A](namedLens)(dCodecA)

  class DeltaCodecOptionalI[A](implicit dCodecA: DeltaCodec[A]) extends DeltaCodec[List[A]] {
    def decoder: Decoder[Delta[List[A]]] = Decoder.instance(c => {
      val o = c.downField("optional").downField("optionalI")
      for {
        index <- o.downField("index").as[Int]
        delta <- o.downField("delta").as[Delta[A]]
      } yield OptionalIDelta[A](OptionalI(index), delta)
    })

    def updateRefs(m: List[A], cache: Cache): List[A] = m.map(a => dCodecA.updateRefs(a, cache))

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

    def updateRefs(m: List[A], cache: Cache): List[A] = m.map(a => dCodecA.updateRefs(a, cache))

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

    def updateRefs(m: Option[A], cache: Cache): Option[A] = m.map(a => dCodecA.updateRefs(a, cache))

    def apply(c: HCursor): Decoder.Result[Delta[Option[A]]] = decoder(c)
  }

  def option[A](implicit dCodecA: DeltaCodec[A]): DeltaCodec[Option[A]] = new DeltaCodecOption[A]

  class DeltaCodecPrismN[S, A](prismN: PrismN[S, A])(implicit dCodecA: DeltaCodec[A]) extends DeltaCodec[S] {

    val decoder: Decoder[Delta[S]] = Decoder.instance(c => {
      c.downField("prism").downField("prismN").downField(prismN.name).as[Delta[A]].map(delta => PrismNDelta[S, A](prismN, delta))
    })

    // Use dCodecA to handle data reached using the prism
    def updateRefs(s: S, cache: Cache): S = prismN.modify(a => dCodecA.updateRefs(a, cache))(s)

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
}
