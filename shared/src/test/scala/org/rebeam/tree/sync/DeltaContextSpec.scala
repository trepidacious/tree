package org.rebeam.tree.sync

import org.rebeam.tree.DeltaContext._
import org.rebeam.tree.sync.Sync._
import org.scalatest._

import scala.language.higherKinds

class DeltaContextSpec extends WordSpec with Matchers {

  "DeltaContextInterpreter" should {
    "provide incrementing Guids" in {
      val dc = for {
        a <- getId[String]
        b <- getId[String]
        c <- getId[String]
      } yield List(a, b, c)

      val cid = ClientId(42)
      val cdid = ClientDeltaId(76)
      val l = DeltaContextInterpreter.run(dc, DeltaId(cid, cdid))

      assert(
        l == List(0, 1, 2).map(Guid[String](cid, cdid, _))
      )
    }
  }

}