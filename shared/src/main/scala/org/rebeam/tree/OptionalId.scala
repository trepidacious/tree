package org.rebeam.tree

import monocle._
import org.rebeam.tree.sync.{Id, Identified}

import scala.language.higherKinds
import scalaz.{Applicative, \/}

case class OptionalId[A <: Identified[A]](id: Id[A]) extends POptional[List[A], List[A], A, A] {

  private val o = Optional[List[A], A](
    _.find(_.id == id)
  )(
    a => {
      list => {
        val i = list.indexWhere(_.id == id)
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
