package org.rebeam.tree

import monocle.{Optional, POptional}
import org.rebeam.tree.ref.Cache
import org.rebeam.tree.ref.Ref

import scalaz.{Applicative, \/}

import scala.language.higherKinds

case class OptionalCache[A](ref: Ref[A]) extends POptional[Cache[A], Cache[A], A, A] {
  lazy val o: Optional[Cache[A], A] = Optional[Cache[A], A](
    c => c(ref)
  )(
    a => c => c.updated(ref.guid, a)
  )

  def getOrModify(s: Cache[A]): Cache[A] \/ A = o.getOrModify(s)

  def set(a: A): Cache[A] => Cache[A] = o.set(a)

  def getOption(s: Cache[A]): Option[A] = o.getOption(s)

  def modifyF[F[_]: Applicative](f: A => F[A])(s: Cache[A]): F[Cache[A]] = o.modifyF(f)(s)

  def modify(f: A => A): Cache[A] => Cache[A] = o.modify(f)
}