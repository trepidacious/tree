package org.rebeam.tree.ot

import scala.annotation.tailrec


sealed trait Atom[A] {
  def inputLengthDelta: Int
  def outputLengthDelta: Int
}

/**
  * An Atom is part of an Operation.
  * Each atom specifies an action that can be pictured in terms of a cursor in the
  * input list, and an output list that accumulates elements.
  * We start with the cursor at the start of the input list, and an empty output list.
  * Each atom moves the cursor and/or accumulates elements in the output list.
  */
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
    require(n > 0)

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
    require(n > 0)

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
    require(l.nonEmpty)

    // Inserting requires no more elements in input, and adds l.size elements to output
    override def inputLengthDelta: Int = 0
    override def outputLengthDelta: Int = l.size
  }
}

/**
  * An operation is a sequence of Atoms, specifying a modification to a List[A].
  *
  * Guarantees an invariant that all operations having the same effect will be equal,
  * by ensuring that:
  *   1.  Consecutive atoms of the same type are merged (Atom(m) + Atom(n) = Atom(m+n), where
  *       m and n are the count for Retain and Delete, and the inserted elements for Insert)
  *   2.  Any (Delete, Insert) pair is swapped to (Insert, Delete), which has the same effect.
  *
  * When 1 and 2 are applied recursively, we can see that we will only be left with operations
  * that consist of a sequence made up of a pattern of subsequences in two classes - either
  * just [Retain] (call this R), or one of [Insert], [Delete] or [Insert, Delete] (call this O).
  * We require that the classes alternate - so we can have [R], [ROROR], [OROR] etc.
  * The Atoms could probably be encoded differently - most obviously by combining insert and delete,
  * but the Retain/Insert/Delete encoding is canonical.
  *
  * Provides a builder-style interface for constructing operations, which maintains
  * invariants.
  *
  * @param atoms        The atoms to be applied to the list, in order
  * @tparam A           The operation will modify lists of this element type
  */
case class Operation[A](atoms: List[Atom[A]]) {

  import Atom._

  private def append(a: Atom[A]): Operation[A] = copy(atoms = atoms :+ a)
  private def dropLast(n: Int): Operation[A] = copy(atoms = atoms.dropRight(n))

  /**
    * The operation can be applied to input lists of exactly this size
    */
  lazy val inputSize: Int = atoms.foldLeft(0){case (s, a) => s + a.inputLengthDelta}

  /**
    * The operation will produce lists of exactly this size when applied
    */
  lazy val outputSize: Int = atoms.foldLeft(0){case (s, a) => s + a.outputLengthDelta}

  /**
    * True if operation always produces output equal to input, false otherwise
    * @return True if operation is identity
    */
  def isIdentity: Boolean = atoms match {
    case Nil => true
    case List(Retain(_)) => true
    case _ => false
  }

  /**
    * Retain n elements
    * @param n  The number of elements to retain
    * @return   A new Operation with additional elements retained
    */
  def retain(n: Int): Operation[A] = {
    require(n > 0)
    atoms.lastOption match {
      // If we have a Retain as last operation already, just merge that Retain with the new one,
      // otherwise append a new Retain.
      // We can't merge or swap with either insert or delete.
      case Some(Retain(m))  =>  dropLast(1).append(Retain(m + n))
      case _                =>  append(Retain(n))
    }
  }

  def insert(l: List[A]): Operation[A] = {
    require(l.nonEmpty)
    // First, note that all inserts leave base length unaltered, and add the length of the
    // insert to target length.
    // Look at the last two atoms, if there are any.
    (atoms.lift(atoms.size - 2), atoms.lastOption) match {

      // Last atom is Insert. We know we can't have last two elements as Delete, Insert since
      // we will never build this sequence, so just merge the Insert to make a new valid sequence
      case (_, Some(Insert(m))) =>
        dropLast(1).append(Insert(m ++ l))

      // We have an Insert, Delete pair, we can merge with the insert
      case (Some(Insert(m)), Some(Delete(d))) =>
        dropLast(2).append(Insert(m ++ l)).append(Delete(d))

      // We have last element delete, and we have already excluded case where we have Insert, Delete, so
      // we need to replace the Delete with our new insert, then append the delete to the end to
      // form a valid Insert, Delete pair
      case (_, Some(Delete(d))) =>
        dropLast(1).append(Insert(l)).append(Delete(d))

      // We have now excluded all sequences ending with an Insert or Delete, so there is nothing we can swap
      // or merge with - just append a new Insert
      case _ =>
        append(Insert(l))
    }
  }

  def delete(n: Int): Operation[A] = {
    require(n > 0)
    atoms.lastOption match {

      // If we have a Delete as last operation already, merge with it.
      // We require an additional n characters in input string to do this,
      // target length is not affected
      case Some(Delete(m)) =>
        dropLast(1).append(Delete(m + n))

      // Otherwise add new delete - we can't merge with a Delete, and if we have an Insert then we know
      // it is not preceded by a Delete, so we add the Delete to make an Insert, Delete pair.
      case _ =>
        append(Delete(n))
    }
  }

  /**
    * Apply the operation to an input list
    * @param input  The input list to operate on. Must have size equal to operation's inputSize
    * @return       The output list generated by the operation on the input list.
    */
  def apply(input: List[A]): List[A] = {
    require(input.size == inputSize)
    Operation.applyRec(input, Nil, atoms)
  }

  def inverse(input: List[A]): Operation[A] = {
    require(input.size == inputSize)
    Operation.inverseRec(input, Operation.empty, atoms)
  }

}

object Operation {
  import Atom._

  def empty[A]: Operation[A] = Operation(Nil)

  @tailrec
  private def inverseRec[A](input: List[A], inverse: Operation[A], atoms: List[Atom[A]]): Operation[A] = {
    atoms match {
      case Nil => inverse
      case a :: as => a match {
        case Retain(n) =>
          require(input.size >= n)
          inverseRec(input.drop(n), inverse.retain(n), as)
        case Insert(l) =>
          inverseRec(input, inverse.delete(l.size), as)
        case Delete(n) =>
          require(input.size >= n)
          inverseRec(input.drop(n), inverse.insert(input.take(n)), as)
      }
    }
  }

  @tailrec
  private def applyRec[A](input: List[A], output: List[A], atoms: List[Atom[A]]): List[A] = {
    atoms match {

      // No more operations. Requires that we have processed the entire input.
      case Nil =>
        require(input.isEmpty)
        output

      case a :: as => a match {

        // Copy n elements from input to output, checking we have enough
        case Retain(n) =>
          require(input.size >= n)
          applyRec(input.drop(n), output ++ input.take(n), as)

        // Append elements to output
        case Insert(l) =>
          applyRec(input, output ++ l, as)

        // Skip n elements from input, checking we have enough
        case Delete(n) =>
          require(input.size >= n)
          applyRec(input.drop(n), output, as)
      }
    }
  }

}

object OT {



}
