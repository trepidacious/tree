package org.rebeam.tree

import io.circe.generic.JsonCodec
import org.rebeam.tree.Delta.DeltaIO
import Delta._
import cats.free.Free
import org.rebeam.tree.util.TreeUtils._
import cats.instances.list._
import cats.syntax.traverse._

/**
  * Deltas on a list using a predicate for location
  */
@JsonCodec
sealed trait ListMatchDelta[U, A, D <: Delta[U, A], F <: A => Boolean] extends Delta[U, List[A]]

object ListMatchDelta {

  /**
    * Update the first matched list element by replacing it with a new value
    * @param f    The predicate used to find the matching elements
    * @param a    The item to place at given index
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    * @tparam F   The type of predicate used to find the matching elements
    */
  case class Updated[U, A, D <: Delta[U, A], F <: A => Boolean](f: F, a: A) extends ListMatchDelta[U, A, D, F]  {
    def apply(l: List[A]): DeltaIO[U, List[A]] = pure {
      val i = l.indexWhere(f)
      if (i < 0) l else l.updated(i, a)
    }
  }

  /**
    * Update a list by inserting a new value after the first matching element
    * @param f    The predicate used to find the first matching element
    * @param a    The item to insert at given index
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    * @tparam F   The type of predicate used to find the matching elements
    */
  case class Inserted[U, A, D <: Delta[U, A], F <: A => Boolean](f: F, a: A) extends ListMatchDelta[U, A, D, F]  {
    def apply(l: List[A]): DeltaIO[U, List[A]] = pure {
      val i = l.indexWhere(f)
      if (i < 0)  l else  l.inserted(i, a)
    }
  }

  /**
    * Edit first matching list element using a Delta
    * @param f    The predicate used to find the matching elements
    * @param d    The delta to run on first matching element
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    * @tparam F   The type of predicate used to find the matching elements
    */
  case class Edited[U, A, D <: Delta[U, A], F <: A => Boolean](f: F, d: D) extends ListMatchDelta[U, A, D, F]  {
    def apply(l: List[A]): DeltaIO[U, List[A]] = {
      val i = l.indexWhere(f)
      if (i < 0) {
        pure(l)
      } else {
        d(l(i)).map(b => l.updated(i, b))
      }
    }
  }


  /**
    * Delete first matching list element
    * @param f    The predicate used to find matching elements to delete
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    * @tparam F   The type of predicate used to find the matching elements
    */
  case class Deleted[U, A, D <: Delta[U, A], F <: A => Boolean](f: F) extends ListMatchDelta[U, A, D, F]  {
    def apply(l: List[A]): DeltaIO[U, List[A]] = pure {
      val i = l.indexWhere(f)
      l.deleted(i)
    }
  }


  /**
    * DLens from a List with ListMatchDelta to first matched element in that list
    * with the appropriate item delta
    * @param f    The predicate used to find matching elements
    * @tparam U   The universe type
    * @tparam A   The list element type
    * @tparam D   The type of delta on list elements
    * @return     DLens from list to indexed element
    */
  def toMatch[U, A, D <: Delta[U, A], F <: A => Boolean](f: F):
    DOptional[U, List[A], ListMatchDelta[U, A, D, F], A, D] = ListMatchDOptional(f)

}

case class ListMatchDOptional[U, A, D <: Delta[U, A], F <: A => Boolean](f: F) extends DOptional[U, List[A], ListMatchDelta[U, A, D, F], A, D] {
  def aToB(l: List[A]): Option[A] = l.find(f)
  def eToD(e: D): ListMatchDelta[U, A, D, F] = ListMatchDelta.Edited(f, e)
}