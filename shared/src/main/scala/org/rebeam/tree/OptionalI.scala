package org.rebeam.tree

import monocle._
import monocle.std.list._

import scala.language.higherKinds
import scalaz.{Applicative, \/}

case class OptionalI[A](index: Int) extends POptional[List[A], List[A], A, A] {
  private val o = listIndex[A].index(index)

  def getOrModify(s: List[A]): List[A] \/ A = o.getOrModify(s)

  def set(b: A): List[A] => List[A] = o.set(b)

  def getOption(s: List[A]): Option[A] = o.getOption(s)

  def modifyF[F[_]: Applicative](f: A => F[A])(s: List[A]): F[List[A]] = o.modifyF(f)(s)

  def modify(f: A => A): List[A] => List[A] = o.modify(f)
}
