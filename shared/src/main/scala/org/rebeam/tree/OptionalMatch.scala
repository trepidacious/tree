package org.rebeam.tree

import monocle._

import scala.language.higherKinds
import scalaz.{Applicative, \/}

case class OptionalMatch[A, F <: A => Boolean](f: F) extends POptional[List[A], List[A], A, A] {
  private val o = Optional[List[A], A](
    _.find(f)
  )(
    a => {
      list => {
        val i = list.indexWhere(f)
        if (i < 0) {
          list
        } else {
          list.updated(i, a)
        }
      }
    }
  )

  /** get the target of a [[POptional]] or return the original value while allowing the type to change if it does not match */
  def getOrModify(s: List[A]): List[A] \/ A = o.getOrModify(s)

  /** get the modified source of a [[POptional]] */
  def set(b: A): List[A] => List[A] = o.set(b)

  /** get the target of a [[POptional]] or nothing if there is no target */
  def getOption(s: List[A]): Option[A] = o.getOption(s)

  /** modify polymorphically the target of a [[POptional]] with an Applicative function */
  def modifyF[G[_]: Applicative](f: A => G[A])(s: List[A]): G[List[A]] = o.modifyF(f)(s)

  /** modify polymorphically the target of a [[POptional]] with a function */
  def modify(f: A => A): List[A] => List[A] = o.modify(f)
}
