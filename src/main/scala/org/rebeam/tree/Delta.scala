package org.rebeam.tree

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

case class ValueDelta[M](v: M) extends Delta[M] {
  def apply(m: M): M = v
}
