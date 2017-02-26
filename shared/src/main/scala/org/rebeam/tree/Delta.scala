package org.rebeam.tree

import org.rebeam.lenses._
import monocle._
import monocle.std.option
import org.rebeam.tree.DeltaContext._

trait Delta[M] {
  def apply(m: M): DeltaContext[M]
}

case class LensDelta[A, B](lens: Lens[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): DeltaContext[A] = delta(lens.get(a)).map(lens.set(_)(a)) //  lens.modify(delta.apply)(a)
}

case class OptionalDelta[A, B](optional: Optional[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): DeltaContext[A] =
    optional.getOption(a).fold(
      DeltaContext.pure(a)
    )(
      delta(_).map(optional.set(_)(a))
    )  //optional.modify(delta.apply)(a)
}

case class LensNDelta[A, B](lensN: LensN[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): DeltaContext[A] = delta(lensN.get(a)).map(lensN.set(_)(a)) //lensN.modify(delta.apply)(a)
}

case class ValueDelta[A](v: A) extends Delta[A] {
  def apply(a: A): DeltaContext[A] = pure(v)  //v
}

case class OptionalIDelta[A](optionalI: OptionalI[A], delta: Delta[A]) extends Delta[List[A]] {
  def apply(a: List[A]): DeltaContext[List[A]] =
    optionalI.getOption(a).fold(
      DeltaContext.pure(a)
    )(
      delta(_).map(optionalI.set(_)(a))
    )  //optionalI.modify(delta.apply)(l)
}

case class OptionalMatchDelta[A, F <: A => Boolean](optionalMatch: OptionalMatch[A, F], delta: Delta[A]) extends Delta[List[A]] {
  def apply(a: List[A]): DeltaContext[List[A]] =
    optionalMatch.getOption(a).fold(
      DeltaContext.pure(a)
    )(
      delta(_).map(optionalMatch.set(_)(a))
    )  //optionalMatch.modify(delta.apply)(l)
}

case class OptionDelta[A](delta: Delta[A]) extends Delta[Option[A]] {
//  private lazy val mod: Option[A] => Option[A] = option.some[A].modify(delta.apply)
  def apply(a: Option[A]): DeltaContext[Option[A]] = option.some[A].getOption(a).fold(
    DeltaContext.pure(a)
  )(
    delta(_).map(option.some[A].set(_)(a))
  )  //mod(o)
}

