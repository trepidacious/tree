package org.rebeam.tree

import io.circe.generic.JsonCodec
import org.rebeam.tree.Delta.DeltaIO
import Delta._

/**
  * Delta on an Option
  */
@JsonCodec
sealed trait OptionDelta[U, V <: U, A, D <: Delta[V, A]] extends Delta[U, Option[A]]

object OptionDelta {

  /**
    * Update the Option contents using a delta. If Option is None, it is unaltered
    * @param d  The delta to apply to contents
    * @tparam U   The universe type
    * @tparam V   The universe type for delta on list elements
    * @tparam A   The option contents type
    * @tparam D   The type of delta on option contents
    */
  case class Edited[U, V <: U, A, D <: Delta[V, A]](d: D) extends OptionDelta[U, V, A, D] {
    def apply(l: Option[A]): DeltaIO[U, Option[A]] = l match {
      case None => pure(None)
      case Some(a) => Delta.widenU[U, V, A](d(a)).map(Some(_))
    }
  }

  /**
    * Replace the option with Some(value)
    * @param a    The new contents
    * @tparam U   The universe type
    * @tparam V   The universe type for delta on list elements
    * @tparam A   The option contents type
    * @tparam D   The type of delta on option contents
    */
  case class Updated[U, V <: U, A, D <: Delta[V, A]](a: A) extends OptionDelta[U, V, A, D] {
    def apply(l: Option[A]): DeltaIO[U, Option[A]] = pure(Some(a))
  }

  /**
    * Replace the entire option
    * @param o    The new option
    * @tparam U   The universe type
    * @tparam V   The universe type for delta on list elements
    * @tparam A   The option contents type
    * @tparam D   The type of delta on option contents
    */
  case class Replaced[U, V <: U, A, D <: Delta[V, A]](o: Option[A]) extends OptionDelta[U, V, A, D] {
    def apply(l: Option[A]): DeltaIO[U, Option[A]] = pure(o)
  }

  /**
    * DOptional from an Option with OptionDelta to any value in that Option,
    * with the appropriate contents delta
    * @tparam U   The universe type
    * @tparam V   The universe type for delta on list elements
    * @tparam A   The option contents type
    * @tparam D   The type of delta on option contents
    * @return     DOptional from option to contents
    */
  def toSome[U, V <: U, A, D <: Delta[V, A]]: DOptional[U, Option[A], OptionDelta[U, V, A, D], V, A, D] =
    new DOptional[U, Option[A], OptionDelta[U, V, A, D], V, A, D] {
      def aToB(o: Option[A]): Option[A] = o
      def eToD(e: D): OptionDelta[U, V, A, D] = Edited(e)
    }

}
