package org.rebeam.tree.random

import org.scalatest._
import org.scalatest.prop.Checkers
import java.util.Random

import cats.implicits._

import PRandomState._

class PRandomSpec extends WordSpec with Matchers with Checkers {

  def sameResults(seed: Long): Unit = {

    def sameResult[A](f: (Random => A), g: (PRandom => (PRandom, A))): Unit = {
      // Might as well do this imperatively - Random is mutable anyway
      val r = new java.util.Random(seed)
      var pr = PRandom(seed)
      1 until 100 foreach {
        i =>
          val pNext = g(pr)
          val rNext = f(r)
          assert(rNext === pNext._2)
          pr = pNext._1
      }
    }

    sameResult(_.nextInt(), _.int)

    // Note includes powers of 2
    1 until 100 foreach {
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
    1 until 100 foreach {
      i => sameResult(
        r => (r.nextInt(), r.nextInt(i), r.nextLong(), r.nextBoolean(), r.nextFloat(), r.nextDouble()),
        p => makeValues(i).run(p).value
      )
    }

  }

  "PRandom" should {
    "generate the same results as java.util.Random" in {
      1 until 100 foreach {
        seed => sameResults(seed)
      }
    }
  }

}
