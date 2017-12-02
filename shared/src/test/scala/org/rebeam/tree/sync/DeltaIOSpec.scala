package org.rebeam.tree.sync

import org.rebeam.tree.Delta._
import org.rebeam.tree.sync.Sync._
import org.scalatest._
import DeltaIORun._
import org.rebeam.tree.{DeltaIOContext, Moment}

import scala.language.higherKinds

class DeltaIOSpec extends WordSpec with Matchers {

  "DeltaIORun" should {
    "provide incrementing Guids" in {
      val dc = for {
        a <- getId[String]
        b <- getId[String]
        c <- getId[String]
      } yield List(a, b, c)

      val cid = ClientId(42)
      val cdid = ClientDeltaId(76)
      val context = DeltaIOContext(Moment(94))
      val l = runDeltaIO(dc, context, DeltaId(cid, cdid))

      assert(
        l.data == List(0, 1, 2).map(wdi => Id[String](Guid(cid, cdid, WithinDeltaId(wdi))))
      )
    }

    "provide expected Context" in {
      val dc = for {
        c1 <- getContext
        c2 <- getContext
      } yield List(c1, c2)

      val cid = ClientId(42)
      val cdid = ClientDeltaId(76)
      val context = DeltaIOContext(Moment(94))
      val l = runDeltaIO(dc, context, DeltaId(cid, cdid))

      l.data(0) === context
      l.data(1) === context
    }

    "provide deterministic, repeatable pseudo-random values like java.util.Random" in {

      def check(bound: Int, clientId: Long, clientDeltaId: Long, moment: Long): Unit = {
        val dc = for {
          i <- getPRInt
          iu <- getPRIntUntil(bound)
          l <- getPRLong
          b <- getPRBoolean
          f <- getPRFloat
          d <- getPRDouble
        } yield (i, iu, l, b, f, d)

        val cid = ClientId(42)
        val cdid = ClientDeltaId(76)
        val context = DeltaIOContext(Moment(94))
        val r = runDeltaIO(dc, context, DeltaId(cid, cdid)).data

        val random = new java.util.Random(cid.id ^ cdid.id)
        val e = (random.nextInt(), random.nextInt(bound), random.nextLong, random.nextBoolean(), random.nextFloat(), random.nextDouble())

        assert(r === e)
      }

      check(
        bound = 23,
        clientId = 42,
        clientDeltaId = 76,
        moment = 94
      )

      check(
        bound = 23,
        clientId = 42,
        clientDeltaId = 76,
        moment = 94
      )

      //Shouldn't depend on moment
      check(
        bound = 23,
        clientId = 42,
        clientDeltaId = 76,
        moment = 95
      )

    }

  }

}
