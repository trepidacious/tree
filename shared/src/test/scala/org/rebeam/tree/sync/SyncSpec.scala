package org.rebeam.tree.sync

import org.rebeam.tree.{Delta, DeltaIOContext, Moment}
import org.rebeam.tree.Delta._
import org.rebeam.tree.sync.Sync._
import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class SyncSpec extends WordSpec with Matchers with Checkers {

  implicit val intIdGen: ModelIdGen[Int] = new ModelIdGen[Int] {
    def genId(l: Int) = Some(ModelId(l))
  }

  implicit val intRefAdder: RefAdder[Int] = RefAdder.noOpRefAdder

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

  val addMoment: Delta[Int] = new Delta[Int] {
    override def apply(m: Int): DeltaIO[Int] = for {
      c <- getContext
    } yield m + c.moment.ms.toInt
  }


  // DeltaId coming from the local client
  def dId(clientDeltaId: Long) = DeltaId(clientId, ClientDeltaId(clientDeltaId))

  // DeltaId coming from the remote client
  def rDId(clientDeltaId: Long) = DeltaId(remoteClientId, ClientDeltaId(clientDeltaId))

  def assertClientState(
    cs: ClientState[Int],
    nextClientDeltaId: Long,
    serverModel: ModelAndId[Int],
    pendingDeltas: Seq[DeltaWithIC[Int]],
    model: Int): Unit = {

    assert(cs.id == clientId)
    assert(cs.nextClientDeltaId == ClientDeltaId(nextClientDeltaId))
    assert(cs.serverModel == serverModel)
    assert(cs.pendingDeltas == pendingDeltas)
    assert(cs.model == model)
  }

  private def makeClientStateWithDeltas = {
    val context1 = DeltaIOContext(Moment(11))
    val context2 = DeltaIOContext(Moment(22))
    val cs1 = initialClientState
    // Apply inc and double deltas to modify model and accumulate pending deltas,
    // creating a new clientstate
    val cs3 = cs1.apply(inc, context1)._1.apply(double, context2)._1

    //Check we get the expected new client state
    assertClientState(
      cs = cs3,
      nextClientDeltaId = 2,
      serverModel = initialServerModel,
      pendingDeltas = Vector(
        DeltaWithIC(inc, dId(0), context1),
        DeltaWithIC(double, dId(1), context2)
      ),
      model = 248)
    cs3
  }

  private def makeClientStateWithDeltasUsingContext = {
    val context1 = DeltaIOContext(Moment(11))
    val context2 = DeltaIOContext(Moment(22))
    val cs1 = initialClientState

    // Apply deltas to add context moment ms value to model, twice
    val cs3 = cs1.apply(addMoment, context1)._1.apply(addMoment, context2)._1

    //Check we get the expected new client state
    assertClientState(
      cs = cs3,
      nextClientDeltaId = 2,
      serverModel = initialServerModel,
      pendingDeltas = Vector(
        DeltaWithIC(addMoment, dId(0), context1),
        DeltaWithIC(addMoment, dId(1), context2)
      ),
      model = 156) // = 123 + 11 + 22
    cs3
  }

  "A ClientState" should {
    "apply deltas" in {
      val cs1 = initialClientState

      val context1 = DeltaIOContext(Moment(11))
      val (cs2, dId1) = cs1.apply(inc, context1)
      assert(dId1 == dId(0))
      assertClientState(cs2, 1, initialServerModel, Vector(DeltaWithIC(inc, dId1, context1)), 124)

      val context2 = DeltaIOContext(Moment(22))
      val (cs3, dId2) = cs2.apply(double, context2)
      assert(dId2 == dId(1))
      assertClientState(cs3, 2, initialServerModel, Vector(DeltaWithIC(inc, dId1, context1), DeltaWithIC(double, dId2, context2)), 248)
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

    //TODO copy this and make it also check that we can change the context used to apply the local delta
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
          LocalDelta(dId(0), DeltaIOContext(Moment(0))),
          RemoteDelta(inc, rDId(200), DeltaIOContext(Moment(200)))
        ),
        ModelId(125)
      )

      val cs2 = cs1.update(incrementalUpdate)

      // We expect the client state to remove first pending delta since server has applied
      // it, generating the expected model id, then apply the remaining pending delta,
      // which will double 125 to 250.
      // Note we check that the remaining pending delta still has the same context
      cs2.fold(
        fail(_),
        cs => assertClientState(cs, 2, ModelAndId(125, ModelId(125)), Vector(DeltaWithIC(double, dId(1), DeltaIOContext(Moment(22)))), 250)
      )

    }

    "override client's local context with server context for a local delta, then apply remaining local delta with local context" in {
      // Note this tests two things:
      //  * Client state retains the locally-generated context for each delta, and uses it when applying, UNTIL
      //  * Client receives a context from the server for a locally-generated delta, at which point it replaces the
      //    locally-generated delta

      val cs1 = makeClientStateWithDeltasUsingContext

      // This represents the server having started from the initial server model (123),
      // then having applied the client's first delta with a context different to the
      // client's context (time is 100 rather than 11).
      // This leads to a final server model of 123 + 100 = 223, and since our ModelIdGen for int just
      // yields the model itself as a Long, the server ends up with ModelId(223).
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(0), DeltaIOContext(Moment(100)))
        ),
        ModelId(223)
      )

      val cs2 = cs1.update(incrementalUpdate)

      // We expect the client state to apply the first pending delta using the server's
      // context, and remove it from pening list since server has applied
      // it, generating the expected model id of 223, then apply the remaining pending local delta,
      // which will add the local context's moment of 22, yielding 245.
      // Note we check that the remaining pending delta still has the same context
      cs2.fold(
        fail(_),
        cs => assertClientState(cs, 2, ModelAndId(223, ModelId(223)), Vector(DeltaWithIC(addMoment, dId(1), DeltaIOContext(Moment(22)))), 245)
      )
    }

    "handles context correctly for a mix of local deltas that have been applied by the server, remote deltas, and pending local deltas" in {
      // Note this tests two things:
      //  * Client state retains the locally-generated context for each delta, and uses it when applying, UNTIL
      //  * Client receives a context from the server for a locally-generated delta, at which point it replaces the
      //    locally-generated delta

      val cs1 = makeClientStateWithDeltasUsingContext

      // This represents the server having started from the initial server model (123),
      // then having applied the client's first delta with a context different to the
      // client's context (time is 100 rather than 11), yielding 223, then having applied a remote
      // client's 200th delta (addMoment again, but with time 2000), yielding a final
      // server model of 2223, which becomes the server model id.
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(0), DeltaIOContext(Moment(100))),
          RemoteDelta(addMoment, rDId(200), DeltaIOContext(Moment(2000)))
        ),
        ModelId(2223)
      )

      val cs2 = cs1.update(incrementalUpdate)

      // We expect the client state to apply the first pending delta using the server's
      // context, and remove it from pending list since server has applied
      // it, generating the expected model id of 223, then apply the remote delta to yield 2223, then
      // finally apply the remaining pending local delta, which will add the local context's moment of 22, yielding 2245.
      // Note we check that the remaining pending delta still has the same context
      cs2.fold(
        fail(_),
        cs => assertClientState(cs, 2, ModelAndId(2223, ModelId(2223)), Vector(DeltaWithIC(addMoment, dId(1), DeltaIOContext(Moment(22)))), 2245)
      )
    }


    "do incremental update partially applying pending with remote delta, rejecting incorrect model id" in {
      val cs1 = makeClientStateWithDeltas

      // As for "do incremental update partially applying pending with remote delta", but with incorrect model id
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(0), DeltaIOContext(Moment(0))),
          RemoteDelta(inc, rDId(200), DeltaIOContext(Moment(200)))
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
          LocalDelta(dId(0), DeltaIOContext(Moment(0))),
          LocalDelta(dId(1), DeltaIOContext(Moment(1))),
          RemoteDelta(inc, rDId(200), DeltaIOContext(Moment(2)))
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
          LocalDelta(dId(1), DeltaIOContext(Moment(0))),
          RemoteDelta(inc, rDId(200), DeltaIOContext(Moment(1)))
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

    "fail if first ModelFullUpdate has an invalid id" in {
      val newServerModel = ModelAndId(950, ModelId(590))
      val fullUpdate = ModelFullUpdate(clientId, newServerModel)
      val r = ClientState.fromFirstUpdate(fullUpdate)
      assert(r.isLeft)
    }

    "fail if first update is a ModelIncrementalUpdate" in {
      val incrementalUpdate = ModelIncrementalUpdate(
        initialServerModel.id,
        Vector[UpdateDelta[Int]](
          LocalDelta(dId(1), DeltaIOContext(Moment(0))),
          RemoteDelta(inc, rDId(200), DeltaIOContext(Moment(1)))
        ),
        ModelId(247)
      )
      val r = ClientState.fromFirstUpdate(incrementalUpdate)
      assert(r.isLeft)
    }

  }

  "Guid" should {
    "produce valid string representation" in {
      val g: Guid = Guid(ClientId(1), ClientDeltaId(10), WithinDeltaId(255))
      assert(g.toString == "guid-1-a-ff")
    }

    "parse valid string representation" in {
      val g: Option[Guid] = Guid.fromString("guid-1-a-ff")
      assert(g.contains(Guid(ClientId(1), ClientDeltaId(10), WithinDeltaId(255))))
    }

    "parse ignoring case" in {
      assert(Guid.fromString("guid-1-a-ff") == Guid.fromString("gUiD-1-A-fF"))
    }

    "recovers arbitrary guid from encoding in string" in {
      check((clientId: Long, clientDeltaId: Long, id: Long) => {
        val g: Guid = Guid(ClientId(clientId), ClientDeltaId(clientDeltaId), WithinDeltaId(id))
        val s = g.toString
        val regexMatches = Guid.regex.findFirstIn(s).contains(s)
        val parseRecovers = Guid.fromString(s).contains(g)
        regexMatches && parseRecovers
      })
    }

  }

  "Ref" should {
    "produce valid string representation" in {
      val r: Ref[Unit] = Ref(Id(Guid(ClientId(1), ClientDeltaId(10), WithinDeltaId(255))))
      assert(Ref.toString(r) == "ref-1-a-ff")
    }

    "parse valid string representation" in {
      val r: Option[Ref[Unit]] = Ref.fromString("ref-1-a-ff")
      assert(r.contains(Ref[Unit](Id(Guid(ClientId(1), ClientDeltaId(10), WithinDeltaId(255))))))
    }

    "parse ignoring case" in {
      assert(Ref.fromString[Unit]("ref-1-a-ff") == Ref.fromString[Unit]("ReF-1-A-fF"))
    }

    "recovers arbitrary ref from encoding in string" in {
      check((clientId: Long, clientDeltaId: Long, id: Long) => {
        val r: Ref[Unit] = Ref(Id(Guid(ClientId(clientId), ClientDeltaId(clientDeltaId), WithinDeltaId(id))))
        val s = Ref.toString(r)
        val regexMatches = Ref.regex.findFirstIn(s).contains(s)
        val parseRecovers = Ref.fromString[Unit](s).contains(r)
        regexMatches && parseRecovers
      })
    }

  }

}
