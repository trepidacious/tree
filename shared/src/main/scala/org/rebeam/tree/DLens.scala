package org.rebeam.tree

import monocle.Lens

trait DLens[U, A, D <: Delta[U, A], B, E <: Delta[U, B]] {
  def aToB(a: A): B
  def eToD(e: E): D
}

object DLens {
  def createInstance[U, A, D <: Delta[U, A], B, E <: Delta[U, B]](fromAToB: A => B, fromEToD: E => D): DLens[U, A, D, B, E] =
    new DLens[U, A, D, B, E] {
      def aToB(a: A): B = fromAToB(a)
      def eToD(e: E): D = fromEToD(e)
    }
  def apply[U, A, D <: Delta[U, A], B, E <: Delta[U, B]](lens: Lens[A, B], eToD: E => D): DLens[U, A, D, B, E] =
    createInstance[U, A, D, B, E](lens.get, eToD)
}
