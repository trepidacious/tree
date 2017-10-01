package org.rebeam.tree.ot

/**
  * An Atom is part of an Operation.
  * Each atom specifies an action that can be pictured in terms of a cursor in the
  * input list, and an output list that accumulates elements.
  * We start with the cursor at the start of the input list, and an empty output list.
  * Each atom moves the cursor and/or accumulates elements in the output list.
  *
  * @tparam A The type of element in lists acted on by this atom
  */
sealed trait Atom[A] {

  /**
    * When this Atom is added to an Operation, it changes the length of input list required
    * by that operation by this amount, which is always positive.
    * @return Number of additional elements required in input to add this Atom to an operation
    */
  def inputLengthDelta: Int
  /**
    * When this Atom is added to an Operation, it changes the length of the output list produced
    * by that operation by this amount, which is always positive.
    * @return Number of additional elements produced in output when this Atom is added to an operation
    */
  def outputLengthDelta: Int
}

object Atom {

  /**
    * Retain the next n elements of the list unaltered.
    *
    * This adds the next n elements of input after the cursor to the output, and
    * moves the cursor on by n elements.
    * @param n    The number of elements to retain, must be positive.
    * @tparam A   The type of element in the list
    */
  case class Retain[A] (n: Int) extends Atom[A] {
    require(n > 0, "retain must have n > 0")

    // Retaining requires n more elements in input, and produces n elements in output
    override def inputLengthDelta: Int = n
    override def outputLengthDelta: Int = n
  }

  /**
    * Delete the next n elements.
    *
    * This just moves the cursor on by n elements.
    * @param n    The number of elements to delete, must be positive.
    * @tparam A   The type of element in the list
    */
  case class Delete[A] (n: Int) extends Atom[A]{
    require(n > 0, "delete must have n > 0")

    // Deleting require n more elements in input, and leaves output unaltered
    override def inputLengthDelta: Int = n
    override def outputLengthDelta: Int = 0
  }

  /**
    * Insert new elements.
    *
    * This leaves the cursor unaltered, and adds the new elements to the output list.
    * @param l    The elements to add to the output list, must be non-empty
    * @tparam A   The type of element in the list
    */
  case class Insert[A] (l: List[A]) extends Atom[A] {
    require(l.nonEmpty, "insert must have elements")

    // Inserting requires no more elements in input, and adds l.size elements to output
    override def inputLengthDelta: Int = 0
    override def outputLengthDelta: Int = l.size
  }

}
