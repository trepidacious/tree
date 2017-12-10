package org.rebeam.tree

import monocle.Optional

trait DOptional[U, A, D <: Delta[U, A], B, E <: Delta[U, B]] {
  def aToB(a: A): Option[B]
  def eToD(e: E): D
}

object DOptional {
  def createInstance[U, A, D <: Delta[U, A], B, E <: Delta[U, B]](aToB: A => Option[B], eToD: E => D): DOptional[U, A, D, B, E] =
    new DOptional[U, A, D, B, E] {
      def aToB(a: A): Option[B] = aToB(a)
      def eToD(e: E): D = eToD(e)
    }
  def apply[U, A, D <: Delta[U, A], B, E <: Delta[U, B]](optional: Optional[A, B], eToD: E => D): DOptional[U, A, D, B, E] =
    createInstance[U, A, D, B, E](optional.getOption, eToD)
}
