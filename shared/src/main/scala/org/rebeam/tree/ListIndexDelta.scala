package org.rebeam.tree

import io.circe.generic.JsonCodec
import org.rebeam.tree.Delta.DeltaIO
import Delta._

/**
  * Deltas on a list using index for location
  */
@JsonCodec
sealed trait ListIndexDelta[U, A, D <: Delta[U, A]] extends Delta[U, List[A]]

object ListIndexDelta {

  /**
    * Update a list element by replacing it with a new value
    * @param i    The list index. If this is outside the list
    *             bounds then the list is left unchanged.
    * @param a    The item to place at given index
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    */
  case class Updated[U, A, D <: Delta[U, A]](i: Int, a: A) extends ListIndexDelta[U, A, D]  {
    def apply(l: List[A]): DeltaIO[U, List[A]] = pure (
      if (i < 0 || i >= l.size) {
        l
      } else {
        l.updated(i, a)
      }
    )
  }

  /**
    * Insert a new list element at a given index
    * @param i    The list index. If this is <=0, item is
    *             inserted at list start, if it is >= list size
    *             item is appended to list.
    * @param a    The new list element
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    */
  case class Inserted[U, A, D <: Delta[U, A]](i: Int, a: A) extends ListIndexDelta[U, A, D] {
    def apply(l: List[A]): DeltaIO[U, List[A]] = pure(
      if (i <= 0) {
        a :: l
      } else if (i >= l.size) {
        l ++ List(a)
      } else {
        val (front, back) = l.splitAt(i)
        front ++ List(a) ++ back
      }
    )
  }

  /**
    * Edit a list element at a given index using a Delta
    * @param i    The list index
    * @param d    The delta with which to edit
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    */
  case class Edited[U, A, D <: Delta[U, A]](i: Int, d: D) extends ListIndexDelta[U, A, D] {
    def apply(l: List[A]): DeltaIO[U, List[A]] =
      if (i < 0 || i >= l.size) {
        pure(l)
      } else {
        d(l(i)).map(
          b => {
            val (front, back) = l.splitAt(i)
            front ++ List(b) ++ back
          }
        )
      }
  }

  /**
    * Delete the list element at a given index using a Delta
    * @param i    The list index. If this is outside list bounds, list
    *             is left unchanged.
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    */
  case class Deleted[U, A, D <: Delta[U, A]](i: Int) extends ListIndexDelta[U, A, D] {
    def apply(l: List[A]): DeltaIO[U, List[A]] = pure(
      if (i < 0 || i >= l.size) {
        l
      } else {
        val (front, back) = l.splitAt(i)
        front ++ back.tail
      }
    )
  }

  /**
    * DLens from a List with ListIndexDelta to an indexed item in that list
    * with the appropriate item delta
    * @param i    The list index
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    * @return     DLens from list to indexed element
    */
  def toIndex[U, A, D <: Delta[U, A]](i: Int): DOptional[U, List[A], ListIndexDelta[U, A, D], A, D] = ListIndexDOptional(i)

}

case class ListIndexDOptional[U, A, D <: Delta[U, A]](i: Int) extends DOptional[U, List[A], ListIndexDelta[U, A, D], A, D] {
  def aToB(l: List[A]): Option[A] = l.lift(i)
  def eToD(e: D): ListIndexDelta[U, A, D] = ListIndexDelta.Edited(i, e)
}
