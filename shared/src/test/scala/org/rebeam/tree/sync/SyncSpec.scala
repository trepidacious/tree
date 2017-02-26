package org.rebeam.tree.sync

import org.rebeam.tree.Delta
import org.rebeam.tree.Delta._
import org.scalatest._
import org.rebeam.tree.sync.Sync._

class SyncSpec extends WordSpec with Matchers {

  implicit val intIdGen = new ModelIdGen[Int] {
    def genId(l: Int) = Some(ModelId(l))
  }

  val clientId = ClientId(42)

  val initialServerModel = ModelAndId(123, ModelId(24))

  val initialClientState: ClientState[Int] = {
    ClientState.initial(clientId, initialServerModel)
  }

  val remoteClientId = ClientId(600)

  def add(i: Int): Delta[Int] = new Delta[Int] {
    override def apply(m: Int): DeltaIO[Int] = pure(m + i)
  }

  def multiply(i: Int): Delta[Int] = new Delta[Int] {
    override def apply(m: Int): DeltaIO[Int] = pure(m * i)
  }

  private val inc = add(1)
  private val double = multiply(2)

  // DeltaId coming from the local client
  def dId(clientDeltaId: Long) = DeltaId(clientId, ClientDeltaId(clientDeltaId))

  // DeltaId coming from the remote client
  def rDId(clientDeltaId: Long) = DeltaId(remoteClientId, ClientDeltaId(clientDeltaId))

  def assertClientState(
    cs: ClientState[Int],
    nextClientDeltaId: Long,
    serverModel: ModelAndId[Int],
    pendingDeltas: Seq[DeltaAndId[Int]],
    model: Int): Unit = {

    assert(cs.id == clientId)
    assert(cs.nextClientDeltaId == ClientDeltaId(nextClientDeltaId))
    assert(cs.serverModel == serverModel)
    assert(cs.pendingDeltas == pendingDeltas)
    assert(cs.model == model)
  }

  private def makeClientStateWithDeltas = {
    val cs1 = initialClientState
    val cs3 = cs1.apply(inc)._1.apply(double)._1
    assertClientState(cs3, 2, initialServerModel, Vector(DeltaAndId(inc, dId(0)), DeltaAndId(double, dId(1))), 248)
    cs3
  }

  "A ClientState" should {
    "apply deltas" in {
      val cs1 = initialClientState

      val (cs2, dId1) = cs1.apply(inc)
      assert(dId1 == dId(0))
      assertClientState(cs2, 1, initialServerModel, Vector(DeltaAndId(inc, dId1)), 124)

      val (cs3, dId2) = cs2.apply(double)
      assert(dId2 == dId(1))
      assertClientState(cs3, 2, initialServerModel, Vector(DeltaAndId(inc, dId1), DeltaAndId(double, dId2)), 248)
    }

    "do full update" in {
      val cs1 = makeClientStateWithDeltas

      val newServerModel = ModelAndId(950, ModelId(950))
      val fullUpdate = ModelFullUpdate(clientId, newServerModel)
      val cs2 = cs1.fullUpdate(fullUpdate)

      cs2.fold(
        fail(_),
        cs => assertClientState(cs, 2, newServerModel, Vector.empty, 950)
      )
    }

    "fail on full update with invalid model id" in {
      val cs1 = makeClientStateWithDeltas

      val newServerModel = ModelAndId(950, ModelId(590))
      val fullUpdate = ModelFullUpdate(clientId, newServerModel)
      val cs2 = cs1.fullUpdate(fullUpdate)

      assert(cs2.isLeft)
    }

    "do incremental update partially applying pending with remote delta" in {
      val cs1 = makeClientStateWithDeltas

      // This represents the server having started from the initial server model (123),
      // then having applied the client's first delta (inc, yielding 124), then having
      // applied a remote client's 200th delta (inc, yielding 125). This leads to a final
      // model of 125, and since our ModelIdGen for int just yields the model itself as
      // a Long, the server ends up with ModelId(125).
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(0)),
          RemoteDelta(inc, rDId(200))
        ),
        ModelId(125)
      )

      val cs2 = cs1.update(incrementalUpdate)

      // We expect the client state to remove first pending delta since server has applied
      // it, generating the expected model id, then apply the remaining pending delta,
      // which will double 125 to 250.
      cs2.fold(
        fail(_),
        cs => assertClientState(cs, 2, ModelAndId(125, ModelId(125)), Vector(DeltaAndId(double, dId(1))), 250)
      )

    }

    "do incremental update partially applying pending with remote delta, rejecting incorrect model id" in {
      val cs1 = makeClientStateWithDeltas

      // As for "do incremental update partially applying pending with remote delta", but with incorrect model id
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(0)),
          RemoteDelta(inc, rDId(200))
        ),
        ModelId(123)
      )

      val cs2 = cs1.update(incrementalUpdate)

      assert(cs2.isLeft)

    }

    "do incremental update fully applying pending, then remote" in {
      val cs1 = makeClientStateWithDeltas

      // Apply local deltas 0 and 1 to give 248, then a remote increment to give 249
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(0)),
          LocalDelta(dId(1)),
          RemoteDelta(inc, rDId(200))
        ),
        ModelId(249)
      )

      val cs2 = cs1.update(incrementalUpdate)

      // Client should have no pending deltas
      cs2.fold(
        fail(_),
        cs => assertClientState(cs, 2, ModelAndId(249, ModelId(249)), Vector.empty, 249)
      )

    }

    "do incremental update skipping a local delta, then remote" in {
      val cs1 = makeClientStateWithDeltas

      // Apply local delta 1 to give 246, then a remote increment to give 247.
      // local delta 0 is not listed so is skipped by client
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(1)),
          RemoteDelta(inc, rDId(200))
        ),
        ModelId(247)
      )

      val cs2 = cs1.update(incrementalUpdate)

      // Client should have no pending deltas
      cs2.fold(
        fail(_),
        cs => assertClientState(cs, 2, ModelAndId(247, ModelId(247)), Vector.empty, 247)
      )
    }

    "be produced from first ModelFullUpdate" in {
      val newServerModel = ModelAndId(950, ModelId(950))
      val fullUpdate = ModelFullUpdate(clientId, newServerModel)
      val r = ClientState.fromFirstUpdate(fullUpdate)

      r.fold(
        fail(_),
        cs => assertClientState(cs, 0, newServerModel, Vector.empty, 950)
      )
    }

    "fail on first ModelFullUpdate with invalid id" in {
      val newServerModel = ModelAndId(950, ModelId(590))
      val fullUpdate = ModelFullUpdate(clientId, newServerModel)
      val r = ClientState.fromFirstUpdate(fullUpdate)
      assert(r.isLeft)
    }

    "fail on first ModelIncrementalUpdate" in {
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(1)),
          RemoteDelta(inc, rDId(200))
        ),
        ModelId(247)
      )
      val r = ClientState.fromFirstUpdate(incrementalUpdate)
      assert(r.isLeft)
    }

  }

}
