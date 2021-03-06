package org.rebeam.tree

import monocle._

import cats.Applicative
import scala.{Either => \/}

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

  def getOrModify(s: List[A]): List[A] \/ A = o.getOrModify(s)

  def set(b: A): List[A] => List[A] = o.set(b)

  def getOption(s: List[A]): Option[A] = o.getOption(s)

  def modifyF[G[_]: Applicative](f: A => G[A])(s: List[A]): G[List[A]] = o.modifyF(f)(s)

  def modify(f: A => A): List[A] => List[A] = o.modify(f)
}
