package org.rebeam.tree.ot

import org.scalatest._
import org.scalatest.prop.Checkers
import Atom._
import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalacheck.Prop._

import scala.annotation.tailrec

class OTSpec extends WordSpec with Matchers with Checkers {

  val input1 = "Hello World!"
  val op1a = Operation.empty[Char].retain(6).insert("big wide ".toList).retain(5).delete(1)
  val output1a = "Hello big wide World"

  val op1b = Operation.empty[Char].retain(12).insert(" :)".toList)
  val output1b = "Hello World! :)"

  val output1ab = "Hello big wide World :)"

  implicit def genRetain[A]: Gen[Retain[A]] =
    Gen.choose(1, 10).map(Retain[A])

  implicit def genDelete[A]: Gen[Delete[A]] =
    Gen.choose(1, 10).map(Delete[A])

  implicit def genInsert[A: Gen]: Gen[Insert[A]] =
    Gen.choose(1, 10).flatMap(
      n => containerOfN[List, A](n, implicitly[Gen[A]])
    ).map(l => Insert(l))

  implicit def genAtom[A: Gen]: Gen[Atom[A]] =
    Gen.oneOf[Atom[A]](genRetain[A], genDelete[A], genInsert[A])

  implicit def genInsertOrDelete[A: Gen]: Gen[Atom[A]] =
    Gen.oneOf[Atom[A]](genDelete[A], genInsert[A])

  implicit val genInt: Gen[Int] = choose(0, Integer.MAX_VALUE)

  case class AtomsAndInput[A](atoms: List[Atom[A]], input: List[A])
  // Generate a list of atoms, and a list with the correct length to be an
  // input for an operation on those atoms.
  // We produce a list of atoms so we can test building an operation with
  // those atoms against the atoms themselves
  implicit def genAtomListAndInput[A: Gen]: Gen[AtomsAndInput[A]] = for {
    n <- Gen.choose(1, 100)
    atoms <- containerOfN[List, Atom[A]](n, genAtom[A])
    inputSize = atoms.foldLeft(0){case (sum, atom) => sum + atom.inputLengthDelta}
    elements <- containerOfN[List, A](inputSize, implicitly[Gen[A]])
  } yield AtomsAndInput(atoms, elements)

  case class AtomAndPosition[A](atom: Atom[A], pos: Int)
  implicit def genInsertOrDeleteAndPosition[A: Gen]: Gen[AtomAndPosition[A]] = for {
    atom <- genInsertOrDelete
    pos <- genInt
  } yield AtomAndPosition(atom, pos)

  // Generate an operation on an input length n. This is formed by starting with a retain
  // of all elements, and then folding in from 1 to n/2 + 1 insert or delete operations
  // at random positions.
  private def genOperationN[A: Gen](n: Int): Gen[Operation[A]] = for {
    // Choose a number of inserts and deletes
    atomCount <- Gen.choose(1, n/2 + 1)

    // Now generate the atoms and positions - note we will wrap the position to lie in the
    // input at each stage
    atomsAndPositions <- containerOfN[List, AtomAndPosition[A]](atomCount, genInsertOrDeleteAndPosition)

  // Start from an operation just retaining everything (note we may have an empty input, so retainIfPositive)
  } yield atomsAndPositions.foldLeft(Operation.empty[A].retainIfPositive(n)){
    case (op, atomAndPos) =>
      // We will compose a new operation with current one (op), we want
      // to perform the operation we have at a valid position in the
      // output of the current operation
      atomAndPos.atom match {
        case Insert(l) =>
          // Insert can happen from 0 to length of input, inclusive (at pos = length, we are appending to end)
          val pos = atomAndPos.pos % (op.outputSize + 1)
          // We can have retain of 0 elements in either call - so use retainIfPositive
          val newOp = Operation.empty[A].retainIfPositive(pos).insert(l).retainIfPositive(op.outputSize - pos)
          op.compose(newOp)

        case Delete(d) if op.outputSize > 0=>
          // Delete can happen from 0 to length of input - 1, inclusive (i.e. we need at least one input
          // element left to delete) - we will truncate delete length as necessary
          val pos = atomAndPos.pos % op.outputSize
          // Can only delete what is left
          val d2 = Math.min(d, op.outputSize - pos)
          // We can have retain of 0 elements in either call - so use retainIfPositive
          val newOp = Operation.empty[A].retainIfPositive(pos).delete(d2).retainIfPositive(op.outputSize - pos - d2)
          op.compose(newOp)

        // We don't generate retains, so just ignore
        case _ => op
      }
  }

  // A pair of operations and an input, where we can apply either operation to the input
  case class OperationPairAndInput[A](a: Operation[A], b: Operation[A], input: List[A])

  implicit def genOperationPairAndInput[A: Gen]: Gen[OperationPairAndInput[A]] = for {
    n <- Gen.choose(1, 100)
    input <- containerOfN[List, A](n, implicitly[Gen[A]])
    a <- genOperationN(n)
    b <- genOperationN(n)
  } yield OperationPairAndInput(a, b, input)


  // A pair of operations and an input, where we can apply a then b to the input
  case class OperationComposePairAndInput[A](a: Operation[A], b: Operation[A], input: List[A])

  implicit def genOperationComposePairAndInput[A: Gen]: Gen[OperationComposePairAndInput[A]] = for {
    n <- Gen.choose(1, 100)
    input <- containerOfN[List, A](n, implicitly[Gen[A]])
    a <- genOperationN(n)
    b <- genOperationN(a.outputSize)
  } yield OperationComposePairAndInput(a, b, input)



  @tailrec
  private final def isRORORec[A](atoms: List[Atom[A]]): Boolean = {
    atoms match {

      //Each case for "RO" section
      case Retain(_) :: Insert(_) :: Delete(_) :: as => isRORORec(as)
      case Retain(_) :: Insert(_) :: as => isRORORec(as)
      case Retain(_) :: Delete(_) :: as => isRORORec(as)

      //Final single retain is fine
      case Retain(_) :: Nil => true

      //Nothing left, so all sections must have been fine
      case Nil => true

      //We have a non-permitted sequence
      case _ => false
    }
  }

  private final def isRORO[A](op: Operation[A]): Boolean = {
    op.atoms match {

      // Start is special case - we can have an "O" section
      // at the start without an initial R, then we hand over
      // to normal rules for the rest of the atoms
      case Insert(_) :: Delete(_) :: as => isRORORec(as)
      case Insert(_) :: as => isRORORec(as)
      case Delete(_) :: as => isRORORec(as)

      // Must have an initial R or be Nil, so we can let
      // recursive method at it
      case as => isRORORec(as)
    }
  }

  "Atoms" should {
    "give correct insertion deltas" in {
      val i = Insert(List(1,2,3))
      assert(i.inputLengthDelta == 0)
      assert(i.outputLengthDelta == 3)
    }
    "give correct retain deltas" in {
      val i = Retain(4)
      assert(i.inputLengthDelta == 4)
      assert(i.outputLengthDelta == 4)
    }
    "give correct delete deltas" in {
      val i = Delete(5)
      assert(i.inputLengthDelta == 5)
      assert(i.outputLengthDelta == 0)
    }
    "require positive retain count" in {
      assertThrows[IllegalArgumentException](Retain(-1))
      assertThrows[IllegalArgumentException](Retain(0))
      Retain(1)
    }
    "require non empty insert size" in {
      assertThrows[IllegalArgumentException](Insert(List.empty[Int]))
      Insert(List(1))
    }
    "require positive delete count" in {
      assertThrows[IllegalArgumentException](Delete(-1))
      assertThrows[IllegalArgumentException](Delete(0))
      Delete(1)
    }
  }

  "Operations " should {
    "apply simple case 1a" in {
      assert(
        op1a(input1.toList).mkString == output1a
      )
    }

    "invert simple case 1a" in {
      assert(
        op1a.inverse(input1.toList).apply(output1a.toList).mkString == input1
      )
    }

    "apply simple case 1b" in {
      assert(
        op1b(input1.toList).mkString == output1b
      )
    }

    "invert simple case 1b" in {
      assert(
        op1b.inverse(input1.toList).apply(output1b.toList).mkString == input1
      )
    }

    "transform and apply simple case 1 concurrent ops a and b" in {
      val (ap, bp) = Operation.transform(op1a, op1b)
      assert(ap(output1b.toList) == output1ab.toList)
      assert(bp(output1a.toList) == output1ab.toList)
    }

    "coalesce retains" in {
      assert(
        Operation.empty[Int].retain(1).retain(100).atoms
          == Operation.empty[Int].retain(101).atoms
      )
    }

    "coalesce deletes" in {
      assert(
        Operation.empty[Int].delete(1).delete(100).atoms
          == Operation.empty[Int].delete(101).atoms
      )
    }

    "coalesce inserts" in {
      assert(
        Operation.empty[Int].insert(List(1)).insert(List(2,3,4)).atoms
          == Operation.empty[Int].insert(List(1,2,3,4)).atoms
      )
    }

    "always have `RORO` invariant" in {
      check{
        forAll(genAtomListAndInput[Int]) {
          ai =>
            val op = Operation.fromAtoms(ai.atoms)
            isRORO(op)
        }
      }
    }

    "always give the same result of correct length when coalesced as when using all atoms" in {
      check{
        forAll(genAtomListAndInput[Int]) {
          ai =>
            val op = Operation.fromAtoms(ai.atoms)
            val opAtoms = Operation(ai.atoms)
            val output = op(ai.input)
            val outputAtoms = opAtoms(ai.input)

            (output == outputAtoms) &&
              (output.size == op.outputSize) &&
              (outputAtoms.size == opAtoms.outputSize)
        }
      }
    }

    "always have inverse such that a.inverse(a(i)) == i for input i" in {
      check{
        forAll(genAtomListAndInput[Int]) {
          ai =>
            val op = Operation.fromAtoms(ai.atoms)
            val inverse = op.inverse(ai.input)
            inverse(op(ai.input)) == ai.input
        }
      }
    }

    "always compose so that a.compose(b)(i) == b(a(i)) for input i" in {
      try {
        check(
          forAll(genOperationComposePairAndInput[Int]) {

            p => p.a.compose(p.b)(p.input) == p.b(p.a(p.input))

          }, MinSuccessful(10000)
        )
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }

    "always transform pair (a, b) to (ap, bp) such that bp(a(i)) == ap(b(i)) for input i" in {
      try {
        check(
          forAll(genOperationPairAndInput[Int]) {
            p =>
              // Transform the pair
              val a = p.a
              val b = p.b
              val (ap, bp) = Operation.transform(a, b)
              val input = p.input

              bp(a(input)) == ap(b(input))

          }, MinSuccessful(10000)
        )
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }

    "require positive retain count" in {
      assertThrows[IllegalArgumentException](Operation.empty[Int].retain(-1))
      assertThrows[IllegalArgumentException](Operation.empty[Int].retain(0))
      Operation.empty[Int].retain(1)
    }

    "require non-negative retainIfPositive count" in {
      assertThrows[IllegalArgumentException](Operation.empty[Int].retainIfPositive(-1))
      Operation.empty[Int].retainIfPositive(0)
      Operation.empty[Int].retainIfPositive(1)
    }

    "require non empty insert size" in {
      assertThrows[IllegalArgumentException](Operation.empty[Int].insert(List.empty[Int]))
      Operation.empty[Int].insert(List(1))
    }

    "require positive delete count" in {
      assertThrows[IllegalArgumentException](Operation.empty[Int].delete(-1))
      assertThrows[IllegalArgumentException](Operation.empty[Int].delete(0))
      Operation.empty[Int].delete(1)
    }

    "be identity with no atoms" in {
      assert(Operation.empty[Int].isIdentity)
    }

    "be identity with only retains" in {
      assert(Operation.empty[Int].retain(3).isIdentity)
      assert(Operation.empty[Int].retain(100).retain(1000).isIdentity)
    }

    "not be identity with non-retain atoms" in {
      assert(!Operation.empty[Int].insert(List(1)).isIdentity)
      assert(!Operation.empty[Int].retain(100).insert(List(1, 2)).isIdentity)
      assert(!Operation.empty[Int].delete(1).isIdentity)
      assert(!Operation.empty[Int].retain(100).delete(100).isIdentity)
      assert(!Operation.empty[Int].insert(List(1, 2)).delete(100).isIdentity)
    }

  }
}
