package org.rebeam.tree

import cats.free.Free
import cats.free.Free.liftF
import io.circe.generic.JsonCodec
import org.rebeam.lenses._
import monocle._
import monocle.std.option
import org.rebeam.tree.Delta._
import org.rebeam.tree.ref.Cache
import org.rebeam.tree.sync.Sync.Guid

import scala.reflect.ClassTag

class WidenedDelta[C, A, B >: A](aDelta: Delta[C, A], f: B => Option[A]) extends Delta[C, B] {
  def apply(b: B): DeltaIO[C, B] = f(b).fold(
    pure[C, B](b)
  )(
    (a: A) => {
      val ad: DeltaIO[C, A] = aDelta (a)
      ad.map (r => r: B)
    }
  )
}

trait Delta[C, A] {
  def apply(a: A): DeltaIO[C, A]

//  def widen[B >: A](f: B => Option[A]): Delta[B] = new WidenedDelta[A, B](this, f)
//  def widenPF[B >: A](f: PartialFunction[B, A]): Delta[B] = new WidenedDelta[A, B](this, f.lift)
//  def widenCT[B >: A](implicit ct: ClassTag[A]): Delta[B] = new WidenedDelta[A, B](this, ct.unapply)
//
}

sealed trait DeltaIOA[C, A]
case class GetId[T]() extends DeltaIOA[Nothing, Guid[T]]
case object GetContext extends DeltaIOA[Nothing, DeltaIOContext]
case class PutData[C](id: Guid[C], c: C) extends DeltaIOA[C, Unit]

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

  type DeltaIO[C, A] = Free[({type L[X] = DeltaIOA[C, X]})#L, A]

  type DeltaIOANothing[A] = DeltaIOA[Nothing, A]

  def identity[C, A] = new Delta[C, A]{
    def apply(a: A): DeltaIO[C, A] = pure(a)
  }

  /**
    * GetId returns a Guid[T] value.
    * @tparam T The type for which we need a Guid
    * @return A new Guid for the given type. Use for
    *         only one data item - do not reuse.
    */
  def getId[T]: DeltaIO[Nothing, Guid[T]] = {
    liftF[DeltaIOANothing, Guid[T]](GetId[T]())
  }

  /**
    * Get the DeltaIOContext within which the DeltaIO
    * is running, for example giving the current time
    * as a Moment.
    */
  val getContext: DeltaIO[Nothing, DeltaIOContext] =
    liftF[DeltaIOANothing, DeltaIOContext](GetContext)

  def putData[C, A](id: Guid[C], c: C): DeltaIO[C, Unit] =
    liftF[({type L[X] = DeltaIOA[C, X]})#L, Unit](PutData[C](id, c))

  def pure[C, A](a: A): DeltaIO[C, A] = Free.pure[({type L[X] = DeltaIOA[C, X]})#L, A](a)

}

case class LensDelta[C, A, B](lens: Lens[A, B], delta: Delta[C, B]) extends Delta[C, A] {
  def apply(a: A): DeltaIO[C, A] = delta(lens.get(a)).map(lens.set(_)(a)) //  lens.modify(delta.apply)(a)
}

case class OptionalDelta[C, A, B](optional: Optional[A, B], delta: Delta[C, B]) extends Delta[C, A] {
  def apply(a: A): DeltaIO[C, A] =
    optional.getOption(a).fold(
      pure[C, A](a)
    )(
      delta(_).map(optional.set(_)(a))
    )  //optional.modify(delta.apply)(a)
}

case class LensNDelta[C, A, B](lensN: LensN[A, B], delta: Delta[C, B]) extends Delta[C, A] {
  def apply(a: A): DeltaIO[C, A] = delta(lensN.get(a)).map(lensN.set(_)(a)) //lensN.modify(delta.apply)(a)
}

case class ValueDelta[C, A](v: A) extends Delta[C, A] {
  def apply(a: A): DeltaIO[C, A] = pure(v)  //v
}

case class OptionalIDelta[C, A](optionalI: OptionalI[A], delta: Delta[C, A]) extends Delta[C, List[A]] {
  def apply(a: List[A]): DeltaIO[C, List[A]] =
    optionalI.getOption(a).fold(
      pure[C, List[A]](a)
    )(
      delta(_).map(optionalI.set(_)(a))
    )  //optionalI.modify(delta.apply)(l)
}

case class OptionalMatchDelta[C, A, F <: A => Boolean](optionalMatch: OptionalMatch[A, F], delta: Delta[C, A]) extends Delta[C, List[A]] {
  def apply(a: List[A]): DeltaIO[C, List[A]] =
    optionalMatch.getOption(a).fold(
      pure[C, List[A]](a)
    )(
      delta(_).map(optionalMatch.set(_)(a))
    )  //optionalMatch.modify(delta.apply)(l)
}

case class OptionDelta[C, A](delta: Delta[C, A]) extends Delta[C, Option[A]] {
//  private lazy val mod: Option[A] => Option[A] = option.some[A].modify(delta.apply)
  def apply(a: Option[A]): DeltaIO[C, Option[A]] = option.some[A].getOption(a).fold(
    pure[C, Option[A]](a)
  )(
    delta(_).map(option.some[A].set(_)(a))
  )  //mod(o)
}

case class PrismNDelta[C, S, A](prismN: PrismN[S, A], delta: Delta[C, A]) extends Delta[C, S] {
  def apply(s: S): DeltaIO[C, S] =
    prismN.getOption(s).fold(
      pure[C, S](s)
    )(
      a => delta(a).map(modifiedA => prismN.set(modifiedA)(s))
    )  //optionalI.modify(delta.apply)(l)
}

case class CacheDelta[C, A](optionalCache: OptionalCache[A], delta: Delta[C, A]) extends Delta[C, Cache[A]] {
  def apply(c: Cache[A]): DeltaIO[C, Cache[A]] =
    optionalCache.getOption(c).fold(
      pure[C, Cache[A]](c)
    )(
      a => delta(a).map(modifiedA => optionalCache.set(modifiedA)(c))
    )
}