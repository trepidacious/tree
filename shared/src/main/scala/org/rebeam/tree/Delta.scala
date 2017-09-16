package org.rebeam.tree

import cats.free.Free
import cats.free.Free.liftF
import io.circe.generic.JsonCodec
import org.rebeam.lenses._
import monocle._
import monocle.std.option
import org.rebeam.tree.Delta._
import org.rebeam.tree.ref.{Mirror, MirrorCodec, Ref}
import org.rebeam.tree.sync.Sync._

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
case class GetId[T]() extends DeltaIOA[Guid[T]]
case object GetContext extends DeltaIOA[DeltaIOContext]
case object GetDeltaId extends DeltaIOA[DeltaId]
//case class GetRandom
case class Put[T](create: Guid[T] => DeltaIO[T], codec: MirrorCodec[T]) extends DeltaIOA[T]

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
    * GetId returns a Guid[T] value.
    * @tparam T The type for which we need a Guid
    * @return A new Guid for the given type. Use for
    *         only one data item - do not reuse.
    */
  def getId[T]: DeltaIO[Guid[T]] =
    liftF[DeltaIOA, Guid[T]](GetId[T]())

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
    * Put a new data item into the Mirror, where that data item
    * can be created by a DeltaIO using a new Guid.
    * This atomically produces a new Guid for a data item, creates
    * that data item, and registers the data item to the Mirror.
    * @tparam T       The type of data item
    * @param create   A function accepting a new Guid for the data item,
    *                 and returning a DeltaIO that will make the data item.
    * @return         A DeltaIO yielding the new item
    */
  def put[T](create: Guid[T] => DeltaIO[T])(implicit codec: MirrorCodec[T]): DeltaIO[T] =
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
  def putPure[T](create: Guid[T] => T)(implicit codec: MirrorCodec[T]): DeltaIO[T] =
    put[T]((id: Guid[T]) => pure(create(id)))

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

case class LensNDelta[A, B](lensN: LensN[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): DeltaIO[A] = delta(lensN.get(a)).map(lensN.set(_)(a)) //lensN.modify(delta.apply)(a)
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

case class OptionDelta[A](delta: Delta[A]) extends Delta[Option[A]] {
//  private lazy val mod: Option[A] => Option[A] = option.some[A].modify(delta.apply)
  def apply(a: Option[A]): DeltaIO[Option[A]] = option.some[A].getOption(a).fold(
    pure(a)
  )(
    delta(_).map(option.some[A].set(_)(a))
  )  //mod(o)
}

case class PrismNDelta[S, A](prismN: PrismN[S, A], delta: Delta[A]) extends Delta[S] {
  def apply(s: S): DeltaIO[S] =
    prismN.getOption(s).fold(
      pure(s)
    )(
      a => delta(a).map(modifiedA => prismN.set(modifiedA)(s))
    )  //optionalI.modify(delta.apply)(l)
}

case class MirrorDelta[A: MirrorCodec](ref: Ref[A], delta: Delta[A]) extends Delta[Mirror] {
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