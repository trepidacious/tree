package org.rebeam.tree

import cats.free.Free
import cats.free.Free.liftF
import io.circe.generic.JsonCodec
import monocle._
import monocle.std.option
import org.rebeam.tree.Delta._
import org.rebeam.tree.ref._
import org.rebeam.tree.sync.Sync._
import org.rebeam.tree.sync._

import scala.reflect.ClassTag

class WidenedDelta[A, B >: A](aDelta: Delta[A], f: B => Option[A]) extends Delta[B] {
  def apply(b: B): DeltaIO[B] = f(b).fold(
    pure(b)
  )(
    a => aDelta(a).map(r => r: B)
  )
}

trait Delta[A] {
  def apply(a: A): DeltaIO[A]

  def widen[B >: A](f: B => Option[A]): Delta[B] = new WidenedDelta[A, B](this, f)
  def widenPF[B >: A](f: PartialFunction[B, A]): Delta[B] = new WidenedDelta[A, B](this, f.lift)
  def widenCT[B >: A](implicit ct: ClassTag[A]): Delta[B] = new WidenedDelta[A, B](this, ct.unapply)

}

sealed trait DeltaIOA[A]
case object GetGuid extends DeltaIOA[Guid]
case class GetId[T]() extends DeltaIOA[Id[T]]
case object GetContext extends DeltaIOA[DeltaIOContext]
case object GetDeltaId extends DeltaIOA[DeltaId]
case object GetPRInt extends DeltaIOA[Int]
case class GetPRIntUntil(bound: Int) extends DeltaIOA[Int]
case object GetPRLong extends DeltaIOA[Long]
case object GetPRBoolean extends DeltaIOA[Boolean]
case object GetPRFloat extends DeltaIOA[Float]
case object GetPRDouble extends DeltaIOA[Double]
case class Put[T](create: Id[T] => DeltaIO[T], codec: MirrorCodec[T]) extends DeltaIOA[T]

/**
  * Provides the context within which an individual run of a DeltaIO can
  * occur. Currently just the moment in which we should run, but may be extended
  * in future.
  * @param moment The moment in which this DeltaIO is running.
  *  When run on the client this is provisional,
  *  on the server it is authoritative - the authoritative
  *  value is returned back to the client when the delta
  *  is applied on the server, so the client can rerun
  *  with the correct moment.)
  */
@JsonCodec
case class DeltaIOContext(moment: Moment)

/**
  * Provides DeltaIOContext for executing DeltaIOs.
  * Note that this has side effects - e.g. reading
  * the current time. This should be used carefully.
  */
trait DeltaIOContextSource {
  def getContext: DeltaIOContext
}

object DeltaIOContextSource {
  val default: DeltaIOContextSource = new DeltaIOContextSource {
    override def getContext: DeltaIOContext = DeltaIOContext(Moment(System.currentTimeMillis()))
  }
}

object Delta {

  type DeltaIO[A] = Free[DeltaIOA, A]

  /**
    * GetId returns an Id[T] value.
    * @tparam T The type for which we need an Id
    * @return A new Id for the given type. Use for
    *         only one data item - do not reuse.
    */
  def getId[T]: DeltaIO[Id[T]] =
    liftF[DeltaIOA, Id[T]](GetId[T]())

  /**
    * GetGuid returns a Guid value.
    * @return A new Guid.
    */
  val getGuid: DeltaIO[Guid] =
    liftF[DeltaIOA, Guid](GetGuid)

  /**
    * Get the DeltaIOContext within which the DeltaIO
    * is running, for example giving the current time
    * as a Moment.
    */
  val getContext: DeltaIO[DeltaIOContext] =
    liftF[DeltaIOA, DeltaIOContext](GetContext)

  /**
    * Get the DeltaId the DeltaIO will produce
    */
  val getDeltaId: DeltaIO[DeltaId] =
    liftF[DeltaIOA, DeltaId](GetDeltaId)

  /**
    * Get a pseudo-random Int. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  val getPRInt: DeltaIO[Int] =
    liftF[DeltaIOA, Int](GetPRInt)

  /**
    * Get a pseudo-random Int between 0 and bound - 1.
    * Produced as if by Random initialised using delta id
    * and then used for all `getPR` calls in that delta.
    * This means the sequence of values will be deterministic
    * for a given DeltaId.
    */
  def getPRIntUntil(bound: Int): DeltaIO[Int] =
    liftF[DeltaIOA, Int](GetPRIntUntil(bound))

  /**
    * Get a pseudo-random Int. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  val getPRLong: DeltaIO[Long] =
    liftF[DeltaIOA, Long](GetPRLong)

  /**
    * Get a pseudo-random Boolean. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  val getPRBoolean: DeltaIO[Boolean] =
    liftF[DeltaIOA, Boolean](GetPRBoolean)

  /**
    * Get a pseudo-random Float. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  val getPRFloat: DeltaIO[Float] =
    liftF[DeltaIOA, Float](GetPRFloat)

  /**
    * Get a pseudo-random Double. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  val getPRDouble: DeltaIO[Double] =
    liftF[DeltaIOA, Double](GetPRDouble)

  /**
    * Put a new data item into the Mirror, where that data item
    * can be created by a DeltaIO using a new Guid.
    * This atomically produces a new Id for a data item, creates
    * that data item, and registers the data item to the Mirror.
    * @tparam T       The type of data item
    * @param create   A function accepting a new Guid for the data item,
    *                 and returning a DeltaIO that will make the data item.
    * @return         A DeltaIO yielding the new item
    */
  def put[T](create: Id[T] => DeltaIO[T])(implicit codec: MirrorCodec[T]): DeltaIO[T] =
    liftF[DeltaIOA, T](Put(create, codec))

  /**
    * Put a new data item into the Mirror, where that data item
    * can be created directly using a new Guid.
    * This atomically produces a new Guid for a data item, creates
    * that data item, and registers the data item to the Mirror.
    * @tparam T       The type of data item
    * @param create   A function accepting a new Guid for the data item,
    *                 and returning a data item.
    * @return         The new item
    */
  def putPure[T](create: Id[T] => T)(implicit codec: MirrorCodec[T]): DeltaIO[T] =
    put[T]((id: Id[T]) => pure(create(id)))

  /**
    * Pure DeltaIO value
    * @param t    The value
    * @tparam T   The type of value
    * @return     DeltaIO returning value directly
    */
  def pure[T](t: T): DeltaIO[T] = Free.pure(t)

}

case class LensDelta[A, B](lens: Lens[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): DeltaIO[A] = delta(lens.get(a)).map(lens.set(_)(a)) //  lens.modify(delta.apply)(a)
}

case class OptionalDelta[A, B](optional: Optional[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): DeltaIO[A] =
    optional.getOption(a).fold(
      pure(a)
    )(
      delta(_).map(optional.set(_)(a))
    )  //optional.modify(delta.apply)(a)
}

case class ValueDelta[A](v: A) extends Delta[A] {
  def apply(a: A): DeltaIO[A] = pure(v)  //v
}

case class OptionalIDelta[A](optionalI: OptionalI[A], delta: Delta[A]) extends Delta[List[A]] {
  def apply(a: List[A]): DeltaIO[List[A]] =
    optionalI.getOption(a).fold(
      pure(a)
    )(
      delta(_).map(optionalI.set(_)(a))
    )  //optionalI.modify(delta.apply)(l)
}

case class OptionalMatchDelta[A, F <: A => Boolean](optionalMatch: OptionalMatch[A, F], delta: Delta[A]) extends Delta[List[A]] {
  def apply(a: List[A]): DeltaIO[List[A]] =
    optionalMatch.getOption(a).fold(
      pure(a)
    )(
      delta(_).map(optionalMatch.set(_)(a))
    )  //optionalMatch.modify(delta.apply)(l)
}

case class OptionalIdDelta[A <: Identified[A]](optionalId: OptionalId[A], delta: Delta[A]) extends Delta[List[A]] {
  def apply(a: List[A]): DeltaIO[List[A]] =
    optionalId.getOption(a).fold(
      pure(a)
    )(
      delta(_).map(optionalId.set(_)(a))
    )
}

case class OptionalRefDelta[A](optionalRef: OptionalRef[A], delta: Delta[Ref[A]]) extends Delta[List[Ref[A]]] {
  def apply(a: List[Ref[A]]): DeltaIO[List[Ref[A]]] =
    optionalRef.getOption(a).fold(
      pure(a)
    )(
      delta(_).map(optionalRef.set(_)(a))
    )
}

case class OptionDelta[A](delta: Delta[A]) extends Delta[Option[A]] {
//  private lazy val mod: Option[A] => Option[A] = option.some[A].modify(delta.apply)
  def apply(a: Option[A]): DeltaIO[Option[A]] = option.some[A].getOption(a).fold(
    pure(a)
  )(
    delta(_).map(option.some[A].set(_)(a))
  )  //mod(o)
}

case class PrismDelta[S, A](prism: Prism[S, A], delta: Delta[A]) extends Delta[S] {
  def apply(s: S): DeltaIO[S] =
    prism.getOption(s).fold(
      pure(s)
    )(
      a => delta(a).map(modifiedA => prism.set(modifiedA)(s))
    )
}

object PrismDelta {
  def classTag[S, A <: S](implicit ct: ClassTag[A]): Prism[S, A] = narrow(ct.unapply)
  def partialNarrow[S, A <: S](pf: PartialFunction[S, A]): Prism[S, A] = Prism(pf.lift)(a => a: S)
  def narrow[S, A <: S](f: S => Option[A]): Prism[S, A] = Prism(f)(a => a: S)
}

case class MirrorDelta[A: MirrorCodec](mirrorType: MirrorType, ref: Ref[A], delta: Delta[A]) extends Delta[Mirror] {
  def apply(mirror: Mirror): DeltaIO[Mirror] =
    mirror(ref).fold(
      // Data is not in mirror, nothing to do
      pure(mirror)
    )(
      // Data is in mirror, apply delta to it
      a => for {
        modifiedA <- delta(a)
        updatedMirror <- mirror.updated(ref.id, modifiedA)
      } yield updatedMirror
    )
}

case class MirrorAndIdDelta[M](d: Delta[Mirror]) extends Delta[MirrorAndId[M]] {
  def apply(m: MirrorAndId[M]): DeltaIO[MirrorAndId[M]] =
    d(m.mirror).map(newMirror => m.copy(mirror = newMirror))
}
