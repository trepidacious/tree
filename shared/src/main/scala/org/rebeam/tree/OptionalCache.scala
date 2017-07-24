//package org.rebeam.tree
//
//import monocle.{Optional, POptional}
//import org.rebeam.tree.ref.Mirror
//import org.rebeam.tree.ref.Ref
//
//import scalaz.{Applicative, \/}
//
//import scala.language.higherKinds
//
//case class OptionalMirror[A](ref: Ref[A]) extends POptional[Mirror, Mirror, A, A] {
//  lazy val o: Optional[Mirror, A] = Optional[Mirror, A](
//    c => c(ref)
//  )(
//    a => c => c.updated(ref.guid, a)
//  )
//
//  def getOrModify(s: Mirror): Mirror \/ A = o.getOrModify(s)
//
//  def set(a: A): Mirror => Mirror = o.set(a)
//
//  def getOption(s: Mirror): Option[A] = o.getOption(s)
//
//  def modifyF[F[_]: Applicative](f: A => F[A])(s: Mirror): F[Mirror] = o.modifyF(f)(s)
//
//  def modify(f: A => A): Mirror => Mirror = o.modify(f)
//}