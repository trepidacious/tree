package org.rebeam.tree

import monocle.{Optional, POptional}
import org.rebeam.tree.ref.Cache
import org.rebeam.tree.sync.Sync.Ref

import scalaz.{Applicative, \/}

import scala.language.higherKinds

case class OptionalCache[M, A <: M](ref: Ref[A]) extends POptional[Cache[M], Cache[M], A, A] {
  lazy val o: Optional[Cache[M], A] = Optional[Cache[M], A](
    c => c(ref)
  )(
    a => c => c.updated(ref.guid, a)
  )

  def getOrModify(s: Cache[M]): Cache[M] \/ A = o.getOrModify(s)

  def set(b: A): Cache[M] => Cache[M] = o.set(b)

  def getOption(s: Cache[M]): Option[A] = o.getOption(s)

  def modifyF[F[_]: Applicative](f: A => F[A])(s: Cache[M]): F[Cache[M]] = o.modifyF(f)(s)

  def modify(f: A => A): Cache[M] => Cache[M] = o.modify(f)
}