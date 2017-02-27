package org.rebeam.tree

import monocle._

import scala.language.higherKinds
import scala.reflect.ClassTag
import scalaz.\/

case class PrismByClass[S, A <: S](implicit ct: ClassTag[A]) extends PPrism[S, S, A, A] {

  private val p = Prism[S, A](ct.unapply)(a => a: S)

  def getOrModify(s: S): S \/ A = p.getOrModify(s)

  def reverseGet(b: A): S = p.reverseGet(b)

  def getOption(s: S): Option[A] = p.getOption(s)

  def classTag: ClassTag[A] = ct

  def name: String = ct.runtimeClass.getSimpleName
}
