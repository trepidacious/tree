package org.rebeam.tree.logoot

import org.rebeam.tree.Delta.DeltaIO
import org.rebeam.tree.{DeltaIOContext, Moment}
import org.rebeam.tree.logoot.Logoot._
import org.rebeam.tree.sync.DeltaIORun
import org.rebeam.tree.sync.Sync._
import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop._
import org.scalacheck.Gen._

import scala.collection.mutable.ListBuffer

class LogootSpec extends WordSpec with Matchers with Checkers {

  def assertCompare[A: Ordering](a: A, b: A, result: Int): Unit =
    assert(implicitly[Ordering[A]].compare(a, b) == result, s" - expected $a compare $b to be $result")
  def assertLessThan[A: Ordering](a: A, b: A): Unit = assertCompare(a, b, -1)
  def assertMoreThan[A: Ordering](a: A, b: A): Unit = assertCompare(a, b, 1)
  def assertSame[A: Ordering](a: A, b: A): Unit = assertCompare(a, b, 0)

  val identifierTests: List[(Identifier, Identifier, Int)] = List(
    // Same
    ( Identifier(0, ClientId(0)),
      Identifier(0, ClientId(0)), 0),

    ( Identifier(1, ClientId(1)),
      Identifier(1, ClientId(1)), 0),

    // Differ in pos, not in client
    ( Identifier(0, ClientId(0)),
      Identifier(1, ClientId(0)), -1),

    ( Identifier(1, ClientId(0)),
      Identifier(0, ClientId(0)), 1),

    // Differ in pos, client differs the other way
    ( Identifier(0, ClientId(1)),
      Identifier(1, ClientId(0)), -1),

    ( Identifier(1, ClientId(0)),
      Identifier(0, ClientId(1)), 1),

    // Same in pos, differ in client
    ( Identifier(0, ClientId(0)),
      Identifier(0, ClientId(1)), -1),

    ( Identifier(0, ClientId(1)),
      Identifier(0, ClientId(0)), 1)
  )

  val deltaContext = DeltaIOContext(Moment(0))
  val deltaId = DeltaId(ClientId(99), ClientDeltaId(999))

  def run[A](d: DeltaIO[A]): A = DeltaIORun.runDeltaIO(d, deltaContext, deltaId).data

  def testAddPositions(l: List[Int], offset: Long, result: List[Int]): Unit = {
    val lb = ListBuffer[Int](l:_*)
    addToPos(offset, lb)
    assert(lb.toList === result)
  }

  case class SmallPosInt(i: Int)
  implicit val arbitraryPosInt: Arbitrary[SmallPosInt] = Arbitrary(Gen.choose(1, 100).map(SmallPosInt))

  def validIdentifier(i: Int, p: Int): Identifier = {
    def noReallyActuallyPositive(x: Int): Int = {
      val abs = Math.abs(x)
      if (abs < 0) 0 else abs
    }
    val pAbs = if (i != 0) {
      Math.max(1, noReallyActuallyPositive(p))
    } else {
      noReallyActuallyPositive(p)
    }
    Identifier(pAbs, ClientId(0))
  }

  implicit val arbitraryPosition: Arbitrary[Position] = Arbitrary(
    for {
      l <- Gen.resize(64, nonEmptyListOf[Int](implicitly[Arbitrary[Int]].arbitrary))
    } yield Position(l.zipWithIndex.map{case (p, i) => validIdentifier(i, p)})
  )

  "Logoot" should {
    "compare identifiers" in {
      for (t <- identifierTests) assertCompare(t._1, t._2, t._3)

    }
    "compare positions" in {
      // Positions with a single identifier compare as per identifier
      for (t <- identifierTests) assertCompare(Position(List(t._1)), Position(List(t._2)), t._3)

      val extraId = Identifier(2, ClientId(0))

      // Positions with different lengths compare by identifiers then by length
      for (t <- identifierTests) assertCompare(Position(List(t._1)), Position(List(t._2, extraId)), if (t._3 != 0) t._3 else -1)
      for (t <- identifierTests) assertCompare(Position(List(t._1, extraId)), Position(List(t._2)), if (t._3 != 0) t._3 else 1)

      // Adding another equal element gives same comparison
      for (t <- identifierTests) assertCompare(Position(List(t._1, extraId)), Position(List(t._2, extraId)), t._3)

      // Empty position is less than any non-empty position
      assertSame(Position.empty, Position.empty)
      for (t <- identifierTests) {
        assertLessThan(Position.empty, Position(List(t._1)))
        assertLessThan(Position.empty, Position(List(t._2)))
      }

      //Check two positions where one is a prefix of the other
      val p = Position(List(
        Identifier(1, ClientId(0)),
        Identifier(1, ClientId(0)),
        Identifier(2, ClientId(0)),
        Identifier(3, ClientId(0)),
        Identifier(4, ClientId(0)),
        Identifier(5, ClientId(0)),
        Identifier(6, ClientId(0)),
        Identifier(7, ClientId(0))
      ))
      val q = Position(List(
        Identifier(1, ClientId(0)),
        Identifier(1, ClientId(0)),
        Identifier(2, ClientId(0)),
        Identifier(3, ClientId(0)),
        Identifier(4, ClientId(0)),
        Identifier(5, ClientId(0)),
        Identifier(6, ClientId(0)),
        Identifier(7, ClientId(0)),
        Identifier(8, ClientId(0))
      ))
      assertLessThan(p, q)

    }

    "add offsets to positions" in {
      //Simple addition to last digit
      testAddPositions(
        l = List(0, 1, 2, 3),
        offset = 1,
        result = List(0, 1, 2, 4)
      )

      //Simple addition to last digit
      testAddPositions(
        l = List(0, 1, 2, 3),
        offset = 2,
        result = List(0, 1, 2, 5)
      )

      //Add two digits, base + 1 is equivalent of 11 in base 10
      testAddPositions(
        l = List(0, 1, 2, 3),
        offset = base + 1,
        result = List(0, 1, 3, 4)
      )

      //Now equivalent of 123
      testAddPositions(
        l = List(0, 1, 2, 3),
        offset = base * base + 2 * base + 3,
        result = List(0, 2, 4, 6)
      )

      //Test carrying - this is equivalent of 0129 in base 10, so adding 1 gets us 0130
      testAddPositions(
        l = List(0, 1, 2, maxPosValue.toInt),
        offset = 1,
        result = List(0, 1, 3, 0)
      )

      //In base 10, 0129 + 2 = 0131
      testAddPositions(
        l = List(0, 1, 2, maxPosValue.toInt),
        offset = 2,
        result = List(0, 1, 3, 1)
      )

      //Test multiple carries - in base 10, 0999 + 1 = 1000
      testAddPositions(
        // This will wrap to first digit when we add 1 - equivalent of 0999 in base 10
        l = List(0, maxPosValue.toInt, maxPosValue.toInt, maxPosValue.toInt),
        offset = 1,
        result = List(1, 0, 0, 0)
      )

      //Test multiple carries on multiple digits of offset, in base 10, 0999 + 12
      testAddPositions(
        l = List(0, maxPosValue.toInt, maxPosValue.toInt, maxPosValue.toInt),
        offset = base + 2,
        result = List(1, 0, 1, 1)
      )

      //Test very large position, 0999...9 + 000...1 = 1000...0
      testAddPositions(
        l = (0 until 100).toList.map(i => if (i == 0) 0 else maxPosValue.toInt),
        offset = 1,
        result = (0 until 100).toList.map(i => if (i == 0) 1 else 0)
      )

      //Test very large position, 0999...9 + 000...2 = 1000...1
      testAddPositions(
        l = (0 until 100).toList.map(i => if (i == 0) 0 else maxPosValue.toInt),
        offset = 2,
        result = (0 until 100).toList.map(i => if (i == 0 || i == 99) 1 else 0)
      )

    }

    "construct positions" in {

      // p and q are 01 and 02, therefore there are no two-digit positions
      // between them - we need to add a digit to get the next position after
      // p and less than q, to get 010. 010 is longer than 01 so is after it.
      // Comparing 010 to q we find that q is larger in the second digit (2 > 1)
      // so q is after 010. We can use 010, 011, 012 etc. up to 01m where m is
      // max value of a pos.
      val p = Position(List(          //01
        Identifier(0, ClientId(0)),
        Identifier(1, ClientId(1))
      ))
      val q = Position(List(          //02
        Identifier(0, ClientId(2)),
        Identifier(2, ClientId(3))
      ))

      // Construct a 3 digit number at offset 0 between p and q.
      // This is 010, and uses the site values from p, except the new 3rd digit which has the provided site value 42
      // Note that we wouldn't actually generate this, since there is no other position between
      // p (01) and this position (010) - 010 is the first 3 digit position after p=01, and any position with more digits
      // would be larger, and any two digit number greater than p=01 is also greater than 010.
      assert(
        constructPosition(size = 3, offset = 0, p, q, ClientId(42))
          ===
        Position(List(Identifier(0, ClientId(0)), Identifier(1, ClientId(1)), Identifier(0, ClientId(42)))) //010
      )

      // Now construct a sensible position: 3 digits, at an offset of 1 from p=01, this is 011.
      // Again inherits sites from p except last
      assert(
        constructPosition(size = 3, offset = 1, p, q, ClientId(42))
          ===
        Position(List(Identifier(0, ClientId(0)), Identifier(1, ClientId(1)), Identifier(1, ClientId(42))))
      )

      // We have space for up to maxPosValue offset from p in the 3rd digit, since it starts at 0, this generates 01m
      assert(
        constructPosition(size = 3, offset = maxPosValue, p, q, ClientId(42))
          ===
          Position(List(Identifier(0, ClientId(0)), Identifier(1, ClientId(1)), Identifier(maxPosValue.toInt, ClientId(42))))
      )

      // r is 03, so q lies between p and r
      val r = Position(List(          //03
        Identifier(0, ClientId(4)),
        Identifier(3, ClientId(5))
      ))

      // p + 1 of size 2 is 02, sites from p except the last
      assert(
        constructPosition(size = 2, offset = 1, p, r, ClientId(42))
          ===
          Position(List(Identifier(0, ClientId(0)), Identifier(2, ClientId(42))))
      )
    }

    "construct positions between other positions" in {

//      val p = Position(List(          //01
//        Identifier(0, ClientId(0)),
//        Identifier(1, ClientId(1))
//      ))
//      val q = Position(List(          //02
//        Identifier(0, ClientId(2)),
//        Identifier(2, ClientId(3))
//      ))
//      val r = Position(List(          //03
//        Identifier(0, ClientId(4)),
//        Identifier(3, ClientId(5))
//      ))
//
//      // There are no 2 digit positions between p and q, so we end up
//      // adding a new digit, at 1
//      assert (
//        run(positionsBetween(p, q, 1))
//          ===
//        List(
//          Position(List(
//            Identifier(0,ClientId(0)),
//            Identifier(1,ClientId(1)),
//            Identifier(1,ClientId(99))
//          ))
//        )
//      )
//
//      // There is a single 1 digit position between p and r, 02
//      assert (
//        run(positionsBetween(p, r, 1))
//          ===
//          List(
//            Position(List(
//              Identifier(0,ClientId(0)),
//              Identifier(2,ClientId(99))
//            ))
//          )
//      )
//
//      val s = Position(List(          //06
//        Identifier(0, ClientId(6)),
//        Identifier(6, ClientId(7))
//      ))
//
//      // Test a gap for 4 new 2 digit codes (from 01 to 06), requesting 4
//      // Will use a step of 1, giving exactly each code
//      assert(
//        run(positionsBetween(p, s, 4))
//          ===
//        List(
//          Position(List(Identifier(0,ClientId(0)), Identifier(2,ClientId(99)))),    //02
//          Position(List(Identifier(0,ClientId(0)), Identifier(3,ClientId(99)))),    //03
//          Position(List(Identifier(0,ClientId(0)), Identifier(4,ClientId(99)))),    //04
//          Position(List(Identifier(0,ClientId(0)), Identifier(5,ClientId(99))))     //05
//        )
//      )
//
//      // Test a gap for 4 new 2 digit codes (from 01 to 06), requesting 3
//      // 3 codes from 4 options means step size 1, so we get the first 3 options
//      assert(
//        run(positionsBetween(p, s, 3))
//          ===
//          List(
//            Position(List(Identifier(0,ClientId(0)), Identifier(2,ClientId(99)))),    //02
//            Position(List(Identifier(0,ClientId(0)), Identifier(3,ClientId(99)))),    //03
//            Position(List(Identifier(0,ClientId(0)), Identifier(4,ClientId(99))))    //04
//          )
//      )
//
//      // Test a gap for 4 new 2 digit codes (from 01 to 06), requesting 2
//      // 2 codes from 4 options means step size 2, so we get the 1st and 3rd option
//      assert(
//        run(positionsBetween(p, s, 2))
//          ===
//          List(
//            Position(List(Identifier(0,ClientId(0)), Identifier(2,ClientId(99)))),    //02
//            Position(List(Identifier(0,ClientId(0)), Identifier(4,ClientId(99))))    //04
//          )
//      )
//
//      // Test a gap for 4 new 2 digit codes (from 01 to 06), requesting 1
//      // 1 codes from 4 options means step size 4, so we get the 1st option only
//      assert(
//        run(positionsBetween(p, s, 1))
//          ===
//          List(
//            Position(List(Identifier(0,ClientId(0)), Identifier(2,ClientId(99))))    //02
//          )
//      )
//
//
//      // Test a gap for 4 new 2 digit codes (from 01 to 06), requesting 5
//      // We therefore require more digits.
//      // Considered as 3 digit codes, 060 - 010 is 5 * base = 5 * 2147483648 = 10737418240
//      // This gives 10737418240 - 1 = 10737418239 options, divided by 5 requested codes
//      // gives a window of 2147483647.8, rounded down to 2147483647 for each code.
//      // The first code is always at + 1, so we go from 01 to 011,
//      // then each subsequent code is at i steps of 2147483647 from this.
//      // 2147483647 is the base - 1, the maximum int value m, so we get 011, then 020, then 02m, then 03(m-1) and so on
//      assert(
//        run(positionsBetween(p, s, 5))
//          ===
//        List(
//          Position(List(Identifier(0,ClientId(0)), Identifier(1,ClientId(1)), Identifier(1,ClientId(99)))),           //011
//          Position(List(Identifier(0,ClientId(0)), Identifier(2,ClientId(99)), Identifier(0,ClientId(99)))),          //020
//          Position(List(Identifier(0,ClientId(0)), Identifier(2,ClientId(99)), Identifier(2147483647,ClientId(99)))), //02m
//          Position(List(Identifier(0,ClientId(0)), Identifier(3,ClientId(99)), Identifier(2147483646,ClientId(99)))), //03(m-1)
//          Position(List(Identifier(0,ClientId(0)), Identifier(4,ClientId(99)), Identifier(2147483645,ClientId(99))))  //04(m-2)
//        )
//      )
//
//      // Test a gap for 1 2-digit code (01 to 03), when requesting 2
//      // We again require more digits, and get 030-010 = 2 * base,
//      // and so 2 * base - 1 = 4294967295 options. Divided by 2 code
//      // we get 2147483647.5 rounding down to 2147483647, so as above
//      // we get 011 then 020
//      assert (
//        run(positionsBetween(p, r, 2))
//          ===
//        List(
//          Position(List(Identifier(0,ClientId(0)), Identifier(1,ClientId(1)), Identifier(1,ClientId(99)))), //011
//          Position(List(Identifier(0,ClientId(0)), Identifier(2,ClientId(99)), Identifier(0,ClientId(99)))) //020
//        )
//      )
//
//      val p = Position(List(
//        Identifier(1, ClientId(0)),
//        Identifier(1, ClientId(1)),
//        Identifier(2, ClientId(2)),
//        Identifier(3, ClientId(3)),
//        Identifier(4, ClientId(4)),
//        Identifier(5, ClientId(5)),
//        Identifier(6, ClientId(6)),
//        Identifier(7, ClientId(7))
//      ))
//      val q = Position(List(
//        Identifier(1, ClientId(8)),
//        Identifier(1, ClientId(9)),
//        Identifier(2, ClientId(10)),
//        Identifier(3, ClientId(11)),
//        Identifier(4, ClientId(12)),
//        Identifier(5, ClientId(13)),
//        Identifier(6, ClientId(14)),
//        Identifier(7, ClientId(15)),
//        Identifier(8, ClientId(16))
//      ))
////      run(positionsBetween(q, p, 2))
//      println(s"$p compare to $q is ${Logoot.positionOrdering.compare(p, q)}")
    }

    "construct valid positions between arbitrary other positions" in {
      check((a: Position, b: Position, n: SmallPosInt) => {

        def test(p: Position, q: Position): Boolean = {
          try {

//            println(s"Inserting ${n.i} positions, p $p, q $q")

            val po = Logoot.positionOrdering
            val ps = run(positionsBetween(p, q, n.i))

            // List must have correct length, be strictly ordered, and lie
            // strictly between p and q
            val rightSize = ps.size == n.i
            val rightOrder = ps.size == 1 || ps.sliding(2).forall(pair => po.compare(pair.head, pair.last) == -1)
            val afterP = po.compare(p, ps.head) == -1
            val beforeQ = po.compare(ps.last, q) == -1

//            println(s"ps = $ps")
//            println(s"rightSize $rightSize, rightOrder $rightOrder, afterP $afterP, beforeQ $beforeQ")

            rightSize && rightOrder && afterP && beforeQ

          // Workaround for not being able to set scalacheck verbosity for some reason...
          // See https://stackoverflow.com/questions/24396407/how-to-display-entire-stack-trace-for-thrown-exceptions-from-scalacheck-tests
          } catch {
            case t: Throwable =>
              println("Failed to generate positions:")
              t.printStackTrace()
              false
          }

        }

        Logoot.positionOrdering.compare(a, b) match {
          case 0 => true
          case -1 => test(a, b)
          case _ => test(b, a)
        }
      }, MinSuccessful(1000))
    }

  }

}
