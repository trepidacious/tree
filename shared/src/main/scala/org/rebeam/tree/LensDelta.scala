package org.rebeam.tree

import monocle.Lens
import org.rebeam.tree.Delta._

//object LensDelta {
//  def byLens[A, B](a: A, lens: Lens[A, B], delta: Delta[B]): DeltaIO[A] = delta(lens.get(a)).map(lens.set(_)(a))
//}
//
//abstract class LensDelta[A, B, D <: Delta[B]](lens: Lens[A, B], d: D) extends Delta[A] {
//  def apply(a: A): DeltaIO[A] = LensDelta.byLens(a, lens, d)
//}

