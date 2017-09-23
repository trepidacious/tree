package org.rebeam.tree.random

import org.scalatest._
import org.scalatest.prop.Checkers
import java.util.Random

import cats.implicits._
import PRandomState._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary._

class PRandomSpec extends WordSpec with Matchers with Checkers {

  def sameResults(seed: Long): Unit = {

    def sameResult[A](f: (Random => A), g: (PRandom => (PRandom, A))): Unit = {
      // Might as well do this imperatively - Random is mutable anyway
      val r = new java.util.Random(seed)
      var pr = PRandom(seed)
      1 to 100 foreach {
        i =>
          val pNext = g(pr)
          val rNext = f(r)
          assert(rNext === pNext._2)
          pr = pNext._1
      }
    }

    sameResult(_.nextInt(), _.int)

    // Note includes powers of 2
    1 to 100 foreach {
      i => sameResult(_.nextInt(i), _.intUntil(i))
    }

    sameResult(_.nextLong(), _.long)
    sameResult(_.nextBoolean(), _.boolean)
    sameResult(_.nextFloat(), _.float)
    sameResult(_.nextDouble(), _.double)

    //TODO use tupled when we update cats
    //Use PRandomState to get our tuple
    def makeValues(bound: Int) = (int, intUntil(bound), long, boolean, float, double).map6((_,_,_,_,_,_))

    // Mix some calls for fun
    1 to 100 foreach {
      i => sameResult(
        r => (r.nextInt(), r.nextInt(i), r.nextLong(), r.nextBoolean(), r.nextFloat(), r.nextDouble()),
        p => makeValues(i).run(p).value
      )
    }

  }

  "PRandom" should {
    "generate the same results as java.util.Random for seeds 0 to 99, and for first 100 values produced" in {
      0 until 100 foreach {
        seed => sameResults(seed)
      }
    }

    "generate an intUntil 772104623 based on seed 99 ^ 999 (case where we must loop in intUntil)" in {
      val pr = PRandom(99 ^ 999)
      val r: Int = pr.intUntil(772104623)._2
      val random = new Random(99 ^ 999)
      val e: Int = random.nextInt(772104623)
      assert(r == e)
    }

    "generate the same results as java.util.Random for arbitrary seeds and bounds" in {
      val gen = for {
        seed <- arbitrary[Long]
        bound <- Gen.posNum[Int]
      } yield (seed, bound)

      check {
        forAll(gen){
          case (seed, bound) =>
            val pr = PRandom(seed)
            val r: Int = pr.intUntil(bound)._2
            val random = new Random(seed)
            val e: Int = random.nextInt(bound)
            sameResults(seed)
            r == e
        }
      }
    }

  }

}
