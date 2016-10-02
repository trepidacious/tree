package org.rebeam.tree

import monocle.{Lens, PLens}

import scalaz.Functor

import scala.language.higherKinds

case class LensN[S, A](name: String, lens: Lens[S, A]) extends PLens[S, S, A, A] {
  def get(s: S): A = lens.get(s)
  def set(b: A): S => S = lens.set(b)
  def modifyF[F[_]: Functor](f: A => F[A])(s: S): F[S] = lens.modifyF(f)(s)
  def modify(f: A => A): S => S = lens.modify(f)
}
