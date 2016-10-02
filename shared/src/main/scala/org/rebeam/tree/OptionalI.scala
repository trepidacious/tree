package org.rebeam.tree

import monocle._
import monocle.std.list._

import scala.language.higherKinds
import scalaz.{Applicative, \/}

case class OptionalI[A](index: Int) extends POptional[List[A], List[A], A, A] {
  private val o = listIndex[A].index(index)

  /** get the target of a [[POptional]] or return the original value while allowing the type to change if it does not match */
  def getOrModify(s: List[A]): List[A] \/ A = o.getOrModify(s)

  /** get the modified source of a [[POptional]] */
  def set(b: A): List[A] => List[A] = o.set(b)

  /** get the target of a [[POptional]] or nothing if there is no target */
  def getOption(s: List[A]): Option[A] = o.getOption(s)

  /** modify polymorphically the target of a [[POptional]] with an Applicative function */
  def modifyF[F[_]: Applicative](f: A => F[A])(s: List[A]): F[List[A]] = o.modifyF(f)(s)

  /** modify polymorphically the target of a [[POptional]] with a function */
  def modify(f: A => A): List[A] => List[A] = o.modify(f)
}
