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
        l.data == List(0, 1, 2).map(Guid[String](cid, cdid, _))
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

  }

}
