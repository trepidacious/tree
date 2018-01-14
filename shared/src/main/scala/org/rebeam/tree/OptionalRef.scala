package org.rebeam.tree

import monocle._
import org.rebeam.tree.sync.{Id, Ref}

import scala.language.higherKinds
import scalaz.{Applicative, \/}

case class OptionalRef[A](id: Id[A]) extends POptional[List[Ref[A]], List[Ref[A]], Ref[A], Ref[A]] {

  private val o = Optional[List[Ref[A]], Ref[A]](
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

  def getOrModify(s: List[Ref[A]]): List[Ref[A]] \/ Ref[A] = o.getOrModify(s)

  def set(b: Ref[A]): List[Ref[A]] => List[Ref[A]] = o.set(b)

  def getOption(s: List[Ref[A]]): Option[Ref[A]] = o.getOption(s)

  def modifyF[G[_]: Applicative](f: Ref[A] => G[Ref[A]])(s: List[Ref[A]]): G[List[Ref[A]]] = o.modifyF(f)(s)

  def modify(f: Ref[A] => Ref[A]): List[Ref[A]] => List[Ref[A]] = o.modify(f)
}
