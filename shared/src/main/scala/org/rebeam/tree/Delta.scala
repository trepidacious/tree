package org.rebeam.tree

import org.rebeam.lenses._
import monocle._
import monocle.std.option

trait Delta[M] {
  def apply(m: M): M
}

case class LensDelta[A, B](lens: Lens[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): A = lens.modify(delta.apply)(a)
}

case class OptionalDelta[A, B](optional: Optional[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): A = optional.modify(delta.apply)(a)
}

case class LensNDelta[A, B](lensN: LensN[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): A = lensN.modify(delta.apply)(a)
}

case class ValueDelta[M](v: M) extends Delta[M] {
  def apply(m: M): M = v
}

case class OptionalIDelta[A](optionalI: OptionalI[A], delta: Delta[A]) extends Delta[List[A]] {
  def apply(l: List[A]) = optionalI.modify(delta.apply)(l)
}

case class OptionalMatchDelta[A, F <: A => Boolean](optionalMatch: OptionalMatch[A, F], delta: Delta[A]) extends Delta[List[A]] {
  def apply(l: List[A]) = optionalMatch.modify(delta.apply)(l)
}

case class OptionDelta[A](delta: Delta[A]) extends Delta[Option[A]] {
  private lazy val mod: Option[A] => Option[A] = option.some[A].modify(delta.apply)
  def apply(o: Option[A]) = mod(o)
}

