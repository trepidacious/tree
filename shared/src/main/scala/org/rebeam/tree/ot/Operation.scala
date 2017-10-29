package org.rebeam.tree.ot

import scala.annotation.tailrec

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
  def isIdentity: Boolean = !atoms.exists {
    case Retain(_) => false
    case _ => true
  }

  /**
    * Retain n elements
    * @param n  The number of elements to retain
    * @return   A new Operation with additional elements retained
    */
  def retain(n: Int): Operation[A] = {
    require(n > 0, "must retain > 0 elements")
    atoms.lastOption match {
      // If we have a Retain as last operation already, just merge that Retain with the new one,
      // otherwise append a new Retain.
      // We can't merge or swap with either insert or delete.
      case Some(Retain(m))  =>  dropLast(1).append(Retain(m + n))
      case _                =>  append(Retain(n))
    }
  }

  /**
    * Retain n elements if n is positive, return same operation if n is zero.
    * @param n The number of elements to retain, or 0 to do nothing
    * @return  Operation with retain
    */
  def retainIfPositive(n: Int): Operation[A] = {
    require(n >= 0, "must retainIfPositive >= 0 elements")

    if (n > 0) {
      retain(n)
    } else {
      this
    }
  }

  def insert(l: List[A]): Operation[A] = {
    require(l.nonEmpty, "cannot insert empty list")
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
    require(n > 0, "must delete > 0 elements")
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
    * Produce a new operation with this operation's effects, then another atom, which may
    * be merged/swapped with existing atoms in this operation.
    * @param atom The atom to run after this operation
    * @return     A new operation with the additional atom
    */
  def andThen(atom: Atom[A]): Operation[A] = atom match {
    case Insert(l) => insert(l)
    case Retain(n) => retain(n)
    case Delete(n) => delete(n)
  }

  /**
    * Apply the operation to an input list
    * @param input  The input list to operate on. Must have size equal to operation's inputSize
    * @return       The output list generated by the operation on the input list.
    */
  def apply(input: List[A]): List[A] = {
    require(input.size == inputSize, s"can only apply to correct input size $inputSize, received ${input.size}")

    val (remainingInput, output) =
      atoms.foldLeft((input, List.empty[A])) {

        // i is remaining input, o is our output, a is the current atom
        case ((i, o), a) => a match {

          // Copy n elements from input to output, checking we have enough
          case Retain(n) =>
            require(i.size >= n, s"only ${i.size} elements in input on retain($n)")
            (i.drop(n), o ++ i.take(n))

          // Append elements to output
          case Insert(l) =>
            (i, o ++ l)

          // Discard n elements from input, checking we have enough
          case Delete(n) =>
            require(i.size >= n, s"only ${i.size} elements in input on delete($n)")
            (i.drop(n), o)
        }
      }

    // Require that operation has used the entire input
    require(remainingInput.isEmpty, "input not empty after operation")

    output
  }

  def inverse(input: List[A]): Operation[A] = {
    require(input.size == inputSize, s"inverse input size ${input.size} != operation input size $inputSize")

    val (remainingInput, inverse) =
      atoms.foldLeft((input, Operation.empty[A])) {

        // i is remaining input, o is our output - the inverse, a is the current atom
        case ((i, o), a) => a match {

          // Retaining is the same in operation and inverse
          case Retain(n) =>
            require(i.size >= n, s"only ${i.size} elements in input on retain($n)")
            (i.drop(n), o.retain(n))

          // Invert inserting by deleting the same number of elements
          case Insert(l) =>
            (i, o.delete(l.size))

          // Invert deleting by reinserting the deleted elements
          case Delete(n) =>
            require(i.size >= n, s"only ${i.size} elements in input on delete($n)")
            (i.drop(n), o.insert(i.take(n)))

        }
      }

    // Require that operation has used the entire input
    require(remainingInput.isEmpty, "input not empty after operation inverted")

    inverse
  }

  /**
    * Compose this operation (a) with another (b), to produce a new operation c that will achieve the same result as
    * applying a then b.
    *
    * This means that:
    *
    * b(a(s)) == (a.compose(b))(s)
    *
    * for any input list s
    *
    * @param b  Another operation
    * @return   An operation equivalent to this operation, then operation b
    */
  def compose(b: Operation[A]): Operation[A] = {
    val a = this

    //a's output size must match b's expected input size
    require(a.outputSize == b.inputSize, s"compose on mismatched output size ${a.outputSize} and input size ${b.inputSize}")

    Operation.composeRec(this.atoms, b.atoms, Operation.empty)
  }

  /**
    * This is just the first element in the pair returned by Operation.transform(this, b).
    * So if we call this `a`, we have:
    *
    * a.after(b)(b(s)) == b.after(a)(a(s)) for any input list s. This is useful for transforming
    * on the server.
    *
    * @param b The operation we wish to apply before this one.
    * @return  This operation transformed so it can be applied after b.
    */
  def after(b: Operation[A]): Operation[A] = Operation.transform(this, b)._1

}

object Operation {
  import Atom._

  def empty[A]: Operation[A] = Operation(Nil)

  def fromAtoms[A](atoms: List[Atom[A]]): Operation[A] = atoms.foldLeft(Operation.empty[A]){
    case (op, atom) => op.andThen(atom)
  }

  private implicit class ListOps[A](list: List[A]) {
    def next: List[A] = list.drop(1)
    def withHead(a: A): List[A] = list.updated(0, a)
  }

  /**
    * This is the basis of operational transform. We take two concurrent operations
    * a and b, that would be applied to the same list of elements, and we produce a pair of new
    * operations(ap, bp), such that:
    *
    * bp(a(s)) = ap(b(s))
    *
    * for any input list s
    *
    * Imagine client alice has applied operation a, and client bob has applied operation b, and we need
    * to bring them back to the same state, preserving both their edits.
    *
    * We can do this by sending bp to alice to apply on top of a, and sending ap to bob to apply on top of b.
    * They will then both end up at the same result, with both of their operations applied.
    *
    * In general, this allows for a system where a server applies operations from a set of clients in the order
    * they are received at the server, but where those operations are transformed so they preserve the intent of the
    * client, allowing for other client's operations that modified the server state concurrently.
    *
    * @param a  The first operation
    * @param b  The second operation
    * @tparam A The type of element in the lists to which operations apply
    * @return   A pair of operations (ap, bp) transformed appropriately from (a, b)
    */
  def transform[A](a: Operation[A], b: Operation[A]): (Operation[A], Operation[A]) = {
    require(a.inputSize == b.inputSize, "transform requires operations to have same input size")
    transformRec(a.atoms, b.atoms, Operation.empty, Operation.empty)
  }

  @tailrec
  def transformRec[A](as1: List[Atom[A]], as2: List[Atom[A]], p1: Operation[A], p2: Operation[A]): (Operation[A], Operation[A]) =

    // At each recursion, we look at what operation 1 and 2 do, and append operations to p1 that will achieve the same
    // effect as operation 1, on the results of operation 2. In the same way we append to p2 operations that will achieve
    // the same effect as operation 2, on the results of operation 1.

    // So for example if operation 1 inserts some elements, these elements will need to be added to p1, adapted as
    // necessary to work with the atoms of operation 2.

    // At each recursion, the cursor associated with both operations must be at the same position in the input list

    (as1.headOption, as2.headOption) match {

      // If we have processed all atoms, we are done
      case (None, None) => (p1, p2)

      // If either operation's atom is an insert, perform the insert on the respective prime operation,
      // and retain the already-inserted elements on the other prime operation. Handle insert in operation 1 first.
      case (Some(Insert(l1)), _) =>
        transformRec(as1.next, as2, p1.insert(l1), p2.retain(l1.size))
      case (_, Some(Insert(l2))) =>
        transformRec(as1, as2.next, p1.retain(l2.size), p2.insert(l2))

      // ERR1 Need operation 1 atom
      case (None, _) =>
        sys.error("Transforming operations, we ran out of atoms in the first operation")

      // ERR2 Need operation 2 atom
      case (_, None) =>
        sys.error("Transforming operations, we ran out of atoms in the second operation")

      // Both operations retain
      case (Some(Retain(n1)), Some(Retain(n2))) =>
        // Operation 1 retains more - split this retain, and append operation 2's smaller retain to prime operations
        if (n1 > n2) {
          transformRec(as1.withHead(Retain(n1 - n2)), as2.next, p1.retain(n2), p2.retain(n2))

        // Same length - just append
        } else if (n1 == n2) {
          transformRec(as1.next, as2.next, p1.retain(n1), p2.retain(n1))

        // Operation 2 retains more - split this retain, and append operation 1's smaller retain to prime operations
        } else {
          transformRec(as1.next, as2.withHead(Retain(n2 - n1)), p1.retain(n1), p2.retain(n1))
        }

      // Both operations delete
      case (Some(Delete(n1)), Some(Delete(n2))) =>
        // Both operations delete the first n2 elements, this needs no additional prime atoms since both operations
        // already do the same thing. But we will need to deal with the extra elements deleted by operation 1.
        if (n1 > n2) {
          transformRec(as1.withHead(Delete(n1 - n2)), as2.next, p1, p2)

        // Both operations delete the same characters - nothing to do in prime operations, each operation already
        // achieves the same thing
        } else if (n1 == n2) {
          transformRec(as1.next, as2.next, p1, p2)

        // Reverse of first case, longer delete on operation 2
        // Split into shared delete of n1, with remainder delete of n2 - n1
        } else {
          transformRec(as1.next, as2.withHead(Delete(n2 - n1)), p1, p2)
        }

      // Delete and retain
      case (Some(Delete(n1)), Some(Retain(n2))) =>
        // operation 1 deletes more elements than operation 2 retains, so we need to split delete
        if (n1 > n2) {
          transformRec(as1.withHead(Delete(n1 - n2)), as2.next, p1.delete(n2), p2)

        // Operation 1 deletes exactly the elements retained by operation 2, so do the delete and ignore the retain
        } else if (n1 == n2) {
          transformRec(as1.next, as2.next, p1.delete(n1), p2)

        // Operation 1 deletes fewer elements than operation 2 retains - so do the delete on p1, and then retain
        // fewer elements on p2
        } else {
          transformRec(as1.next, as2.withHead(Retain(n2 - n1)), p1.delete(n1), p2)
        }

      // Retain and delete, mirror image of delete and retain
      case (Some(Retain(n1)), Some(Delete(n2))) =>
        // operation 1 retains more elements than operation 2 deletes, so we need to split the retain
        if (n1 > n2) {
          transformRec(as1.withHead(Retain(n1 - n2)), as2.next, p1, p2.delete(n2))

        // Operation 1 retains the elements deleted by operation 2, so do the delete and ignore the retain
        } else if (n1 == n2) {
          transformRec(as1.next, as2.next, p1, p2.delete(n2))

        // Operation 2 deletes more elements than retained by operation 1, so split the delete
        } else {
          transformRec(as1.next, as2.withHead(Delete(n2 - n1)), p1, p2.delete(n1))
        }

    }


  @tailrec
  private def composeRec[A](as1: List[Atom[A]], as2: List[Atom[A]], c: Operation[A]): Operation[A] =
    (as1.headOption, as2.headOption) match {

      // Note use of drop(1) throughout - we don't use tail since this
      // gives an exception when list is empty, and we want to just keep the
      // empty list in this case.

      // If we have processed all atoms, we are done
      case (None, None) => c

      // CASE1 Operation 1 wants to delete
      case (Some(Delete(n1)), _) =>
        composeRec(as1.next, as2, c.delete(n1))

      // CASE2 Operation 2 wants to insert
      case (_, Some(Insert(l2))) =>
        composeRec(as1, as2.next, c.insert(l2))

      // ERR1 Need operation 1 atom
      case (None, _) =>
        sys.error("Composing operations, we ran out of atoms in the first operation")

      // ERR2 Need operation 2 atom
      case (_, None) =>
        sys.error("Composing operations, we ran out of atoms in the second operation")

      // CASE3 Both operations want to retain.
      case (Some(Retain(n1)), Some(Retain(n2))) =>
        // Operation 1 wants to retain more, so perform operation 2's retain first, and update
        // operation 1's retain to just do the remainder of the elements
        if (n1 > n2) {
          composeRec(as1.withHead(Retain(n1 - n2)), as2.next, c.retain(n2))

        // Both operations want to retain same number of elements, so we can just do this to satisfy both.
        } else if (n1 == n2) {
          composeRec(as1.next, as2.next, c.retain(n1))

        // Operation 2 wants to retain more, handle as per first case above but using operation 1's retain
        // first, then the remainder of operation 2's
        } else {
          composeRec(as1.next, as2.withHead(Retain(n2 - n1)), c.retain(n1)) //Note n1 not n2!
        }

      // CASE4 Operation 1 insert, and Operation 2 delete
      case (Some(Insert(l1)), Some(Delete(n2))) =>
        // We are inserting more than we are deleting. So operation 1 will insert the full text after
        // current cursor. Then at the same cursor, operation 2 will delete part of that insertion. This
        // is equivalent to just inserting the elements that are not deleted
        if (l1.size > n2) {
          composeRec(as1.withHead(Insert(l1.drop(n2))), as2.next, c)

        // Operation 2 deletes exactly the elements inserted by operation 1
        } else if (l1.size == n2) {
          composeRec(as1.next, as2.next, c)

        // Operation 2 deletes all the elements inserted by operation 1, then some more. Equivalent to just
        // omitting the insert, and deleting correspondingly less elements
        } else {
          composeRec(as1.next, as2.withHead(Delete(n2 - l1.size)), c)
        }

      // CASE5 Operation 1 insert, operation 2 retain
      case (Some(Insert(l1)), Some(Retain(n2))) =>
        // If we are inserting more than will be retained, then we will break up the insert - we need
        // to insert the elements operation2 will then retain, and postpone inserting the rest
        if (l1.size > n2) {
          composeRec(as1.withHead(Insert(l1.drop(n2))), as2.next, c.insert(l1.take(n2)))

        // Operation 2 wants to retain exactly the inserted elements, so we can just do the insert, and this
        // will also handle the retain
        } else if (l1.size == n2) {
          composeRec(as1.next, as2.next, c.insert(l1))

        // We are inserting less than will be retained. Do the insertion, and then we need to retain the
        // remainder of the count requested by operation 2
        } else {
          composeRec(as1.next, as2.withHead(Retain(n2 - l1.size)), c.insert(l1))
        }

      // CASE6 Operation 1 retain, operation 2 delete
      case (Some(Retain(n1)), Some(Delete(n2))) =>

        // Operation 1 wants to retain more elements than operation 2 deletes. Perform the delete,
        // then retain less characters to account for this
        if (n1 > n2) {
          composeRec(as1.withHead(Retain(n1 - n2)), as2.next, c.delete(n2))

        // Operation 1 retains exactly the deleted elements - just delete them, this achieves the retain
        } else if (n1 == n2) {
          composeRec(as1.next, as2.next, c.delete(n2))

        // Operation 1 retains less elements than are deleted. We need to split the delete.
        } else {
          composeRec(as1.next, as2.withHead(Delete(n2 - n1)), c.delete(n1)) //Note n1 not n2!
        }
    }

}
