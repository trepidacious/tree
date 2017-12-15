package org.rebeam.tree

import io.circe.generic.JsonCodec
import org.rebeam.tree.Delta.DeltaIO
import Delta._

/**
  * Delta on an Option
  */
@JsonCodec
sealed trait OptionDelta[U, A, D <: Delta[U, A]] extends Delta[U, Option[A]]

object OptionDelta {

  /**
    * Update the Option contents using a delta. If Option is None, it is unaltered
    * @param d  The delta to apply to contents
    * @tparam U   The universe type
    * @tparam A   The option contents type
    * @tparam D   The type of delta on option contents
    */
  case class Edited[U, A, D <: Delta[U, A]](d: D) extends OptionDelta[U, A, D] {
    def apply(l: Option[A]): DeltaIO[U, Option[A]] = l match {
      case None => pure(None)
      case Some(a) => d(a).map(Some(_))
    }
  }

  /**
    * Replace the option with Some(value)
    * @param a    The new contents
    * @tparam U   The universe type
    * @tparam A   The option contents type
    * @tparam D   The type of delta on option contents
    */
  case class Updated[U, A, D <: Delta[U, A]](a: A) extends OptionDelta[U, A, D] {
    def apply(l: Option[A]): DeltaIO[U, Option[A]] = pure(Some(a))
  }

  /**
    * Replace the entire option
    * @param o    The new option
    * @tparam U   The universe type
    * @tparam A   The option contents type
    * @tparam D   The type of delta on option contents
    */
  case class Replaced[U, A, D <: Delta[U, A]](o: Option[A]) extends OptionDelta[U, A, D] {
    def apply(l: Option[A]): DeltaIO[U, Option[A]] = pure(o)
  }

  /**
    * DOptional from an Option with OptionDelta to any value in that Option,
    * with the appropriate contents delta
    * @tparam U   The universe type
    * @tparam A   The option contents type
    * @tparam D   The type of delta on option contents
    * @return     DOptional from option to contents
    */
  def toSome[U, A, D <: Delta[U, A]]: DOptional[U, Option[A], OptionDelta[U, A, D], A, D] =
    new DOptional[U, Option[A], OptionDelta[U, A, D], A, D] {
      def aToB(o: Option[A]): Option[A] = o
      def eToD(e: D): OptionDelta[U, A, D] = Edited(e)
    }

}
