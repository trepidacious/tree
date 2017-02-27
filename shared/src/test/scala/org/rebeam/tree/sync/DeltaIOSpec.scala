package org.rebeam.tree.sync

import org.rebeam.tree.Delta._
import org.rebeam.tree.sync.Sync._
import org.scalatest._
import DeltaIORun._

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
      val l = runDeltaIO(dc, DeltaId(cid, cdid))

      assert(
        l == List(0, 1, 2).map(Guid[String](cid, cdid, _))
      )
    }
  }

}
