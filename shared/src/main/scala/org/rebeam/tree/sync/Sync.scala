package org.rebeam.tree.sync

import cats.data.Xor
import org.rebeam.tree.Delta

/**
  * Globally unique identifier for a client (globally refers to the whole system under consideration -
  * may just be one server).
  * Assigned by server.
  * @param id The identifier value
  */
case class ClientId(id: Long) extends AnyVal

/**
  * Identifier for a delta, unique for a given client but not globally
  * Assigned by each client for deltas it generates.
  * @param id The identifier value
  */
case class ClientDeltaId(id: Long) extends AnyVal {
  def next: ClientDeltaId = ClientDeltaId(id + 1)
}

/**
  * Identifier for a delta, globally unique (globally refers to the whole system under consideration -
  * may just be one server).
  * @param c  Client id
  * @param d  Id of delta on that client
  */
case class DeltaId(c: ClientId, d: ClientDeltaId)

case class DeltaAndId[A](delta: Delta[A], id: DeltaId)

/**
  * Globally likely-unique identifier for a model revision. This may be an actual guid, or a good
  * quality hash.
  * @param id Identifier
  */
case class ModelId(id: Long) extends AnyVal

case class ModelAndId[A](model: A, id: ModelId)

/**
  * ADT for the two options we can receive from the server representing a delta in an update - either just a
  * DeltaId for a delta that came from this client, or a DeltaId and actual delta for a delta
  * generated by another client
  *
  * @tparam A The model type
  */
sealed trait UpdateDelta[A] {
  def id: DeltaId
}

/**
  * A delta generated remotely, by another client, relayed to us by the server in an update
  * @param delta  The delta
  * @param id     The id of the delta
  * @tparam A     The model type
  */
case class RemoteDelta[A](delta: Delta[A], id: DeltaId) extends UpdateDelta[A]

/**
  * A delta generated locally, by this client, then referenced by the server in an update
  * @param id     The id of the delta
  * @tparam A     The model type
  */
case class LocalDelta[A](id: DeltaId) extends UpdateDelta[A]

/**
  * Sealed trait of model updates sent from server to client
  * @tparam A     The model type
  */
sealed trait ModelUpdate[A]

/**
  * An incremental update received from the server, building a new model based
  * on an existing base model assumed to be held by the client.
  * @param baseModelId    The ModelId of the base model from which we will produce the updated model
  * @param updates        The updates to apply to the base model to produce the updated model
  * @param updatedModelId The ModelId of the updated model
  * @tparam A             The type of model
  */
case class ModelIncrementalUpdate[A](
  baseModelId: ModelId,
  updates: Seq[UpdateDelta[A]],
  updatedModelId: ModelId) extends ModelUpdate[A]

/**
  * A full update received from server, directly setting a new model and overwriting
  * any pending deltas. The first update from the server must always be of this form.
  * @param clientId       The client id for the client. Sets id on first update, and
  *                       may be used to change client id on subsequent full updates.
  * @param model          The full model, and id
  * @tparam A             The type of model
  */
case class ModelFullUpdate[A](
  clientId: ClientId,
  model: ModelAndId[A]) extends ModelUpdate[A]

// Temporary class used to manage a model and deltas
private case class ModelAndDeltas[A](model: A, deltas: Seq[DeltaAndId[A]]) {
  def applyDelta(delta: Delta[A]): ModelAndDeltas[A] = copy(model = delta(model))
  lazy val modelWithDeltas = deltas.foldLeft(model){case (m, d) => d.delta(m)}
}

trait ModelIdGen[A] {
  def genId(model: A): Option[ModelId]
}

object ClientState {
  def initial[A: ModelIdGen](id: ClientId, serverModel: ModelAndId[A]) =
    ClientState(id, ClientDeltaId(0), serverModel, Vector(), serverModel.model)

  /**
    * Create a ClientState from the first update received. Must be a ModelFullUpdate, otherwise
    * this will fail.
    * @param update   The first update
    * @return         A new ClientState or a String error.
    * @tparam A       The type of model
    */
  def fromFirstUpdate[A: ModelIdGen](update: ModelUpdate[A]): Xor[String, ClientState[A]] = update match {
    case i@ModelIncrementalUpdate(_,_,_) => Xor.Left("First ClientState update must be a ModelFullUpdate, got " + i)
    case f@ModelFullUpdate(_,_)  => fromFullUpdate(f, ClientDeltaId(0))
  }

  def fromFullUpdate[A: ModelIdGen](update: ModelFullUpdate[A], nextClientDeltaId: ClientDeltaId): Xor[String, ClientState[A]] = {
    implicitly[ModelIdGen[A]].genId(update.model.model) match {
      case Some(genId) if genId != update.model.id =>
        Xor.Left("Locally-generated id " + genId + " does not match full update's remote-generated id " + update.model.id)
      // Missing id or matched id give success
      case _ => Xor.right(ClientState(id = update.clientId, nextClientDeltaId = nextClientDeltaId, serverModel = update.model, pendingDeltas = Seq.empty, model = update.model.model))
    }
  }
}

/**
  * State for a root component, with data needed by client to synchronise with server.
  *
  * @param id                 This client's id
  * @param nextClientDeltaId  The delta id we will use for the next new local delta
  * @param serverModel        The most recent model received from the server, and its id.
  * @param pendingDeltas      The deltas generated by this client since the most recent server
  *                           model, in the order they were generated and should be applied.
  * @param model              The model currently displayed in the UI, incorporating pendingDeltas
  *                           applied in sequence to serverModel
  * @tparam A                 The type of model
  */
case class ClientState[A: ModelIdGen](id: ClientId, nextClientDeltaId: ClientDeltaId, serverModel: ModelAndId[A], pendingDeltas: Seq[DeltaAndId[A]], model: A) {

  /**
    * Apply a new delta to this client state, to produce a new client state
    * @param delta  The delta to apply
    * @return A new ClientState
    */
  def apply(delta: Delta[A]): (ClientState[A], DeltaId) = {
    val nextModel = delta.apply(model)
    val deltaId = DeltaId(id, nextClientDeltaId)
    val deltaAndId = DeltaAndId(delta, deltaId)
    val state = copy(nextClientDeltaId = nextClientDeltaId.next, model = nextModel, pendingDeltas = pendingDeltas :+ deltaAndId)
    (state, deltaId)
  }

  /**
    * Reconcile a ModelUpdate to this client state, to produce a new client state.
    * @param update The update to apply
    * @return An error xor a new ClientState
    */
  def update(update: ModelUpdate[A]): Xor[String, ClientState[A]] = update match {
    case i@ModelIncrementalUpdate(_,_,_) => incrementalUpdate(i)
    case f@ModelFullUpdate(_,_)  => fullUpdate(f)
  }

  /**
    * Reconcile a full update to this client state, to produce a new client state. This will clear all pendingDeltas
    * and set serverModel and model to the provided full model, and use new client id.
    * @param update The update to apply
    * @return A new ClientState
    */
  def fullUpdate(update: ModelFullUpdate[A]): Xor[String, ClientState[A]] = ClientState.fromFullUpdate(update, nextClientDeltaId)

  /**
    * Reconcile an incremental update to this client state, to produce a new client state, if possible.
    * @param update The update to apply
    * @return An error xor a new ClientState
    */
  def incrementalUpdate(update: ModelIncrementalUpdate[A]): Xor[String, ClientState[A]] = {
    if (update.baseModelId != serverModel.id) {
      Xor.left("Server update expected base model id " + update.baseModelId + " but we have server model id " + serverModel.id)

    } else {

      //Convert the updates into real deltas, requiring that all the LocalDeltas are actually in our pendingDeltas, in the
      //same order. As we do this, produce a new pendingDeltas with all local deltas up to and including the last LocalDelta
      //removed. The logic behind this is that:
      // 1. We require that the server applies our local deltas in the same order we did, although it is is fine for remote deltas
      //    to be interleaved.
      // 2. The server may omit applying local deltas, and if it does, we will discard them too (this might not be the best approach?)
      // 3. We can't apply a LocalDelta unless we know what it is - the server saves bandwidth by just referencing them by id.
      // 4. We keep hold of any pendingDeltas AFTER the most recent LocalDelta since the server may not have received them yet, so
      //    we need to retain them until they are either applied or we see that they are rejected because they are skipped (again,
      //    this might not be the best approach - we could alternatively require an explicit rejection of the delta).
      // 5. The reduced pending delta list can then be used to build our new client model, optimistically applying deltas we
      //    believe are still pending (on their way to the server and back).

      val initialMAD: Xor[String, ModelAndDeltas[A]] = Xor.Right(ModelAndDeltas(serverModel.model, pendingDeltas))

      val updatedMAD = update.updates.foldLeft(initialMAD){
        case (Xor.Right(mad), u) => u match {
          //For remote deltas, just apply them to the model, pending deltas are unaffected
          case RemoteDelta(delta, _) => Xor.Right(mad.applyDelta(delta))

          //For local deltas, find them in pendingDeltas
          case LocalDelta(localDeltaId) =>

            //Drop all deltas before the matching local delta (or drop everything if there is no matching local delta)
            val remainingDeltas = mad.deltas.dropWhile(_.id != localDeltaId)
            remainingDeltas match {
              case delta +: tail => Xor.Right(
                mad.copy(
                  model = delta.delta(mad.model),
                  deltas = tail
                )
              )

              case Nil => Xor.Left("No pending delta for server-specified local id " + localDeltaId )
            }

        }

        //TODO is there a neater way to skip to end here?
        //Failed already - ignore rest of updates
        case (fail, _) => fail
      }

      for {
        mad <- updatedMAD
        //Check the server-provided model id against one generated from our updated model (if we can generate one)
        checkedMad <- implicitly[ModelIdGen[A]].genId(mad.model) match {
          // Fail if we have a genId and it doesn't match
          case Some(genId) if genId != update.updatedModelId =>
            Xor.Left("Locally-generated id " + genId + " does not match remote-generated id " + update.updatedModelId)
          // Missing id or matched id give success
          case _ => Xor.right(mad)
        }
      // Produce updated client state from the checked model and deltas - new model
      // has remaining deltas applied, server model doesn't, pending deltas are those remaining in checkedMAD
      } yield copy(model = checkedMad.modelWithDeltas, serverModel = ModelAndId(checkedMad.model, update.updatedModelId), pendingDeltas = checkedMad.deltas)

    }
  }
}