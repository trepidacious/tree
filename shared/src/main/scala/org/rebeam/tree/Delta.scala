package org.rebeam.tree

import org.rebeam.lenses._
import monocle._

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