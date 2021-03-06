package org.rebeam.tree.sync

import org.rebeam.tree.{Delta, DeltaIOContext}
import org.rebeam.tree.sync.Sync._
import DeltaIORun._

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
  def fromFirstUpdate[A: ModelIdGen](update: ModelUpdate[A]): Either[String, ClientState[A]] = update match {
    case i@ModelIncrementalUpdate(_,_,_) => Left("First ClientState update must be a ModelFullUpdate, got " + i)
    case f@ModelFullUpdate(_,_)  => fromFullUpdate(f, ClientDeltaId(0))
  }

  def fromFullUpdate[A: ModelIdGen](update: ModelFullUpdate[A], nextClientDeltaId: ClientDeltaId): Either[String, ClientState[A]] = {
    implicitly[ModelIdGen[A]].genId(update.model.model) match {
      case Some(genId) if genId != update.model.id =>
        Left("Locally-generated id " + genId + " does not match full update's remote-generated id " + update.model.id)
      // Missing id or matched id give success
      case _ => Right(
        ClientState(
          id = update.clientId,
          nextClientDeltaId = nextClientDeltaId,
          serverModel = update.model,
          pendingDeltas = Seq.empty,
          model = update.model.model
        )
      )
    }
  }

  // Temporary class used to manage a model and deltas
  private case class ModelAndDeltas[A](model: A, deltas: Seq[DeltaWithIC[A]]) {
    def applyDelta(delta: Delta[A], deltaId: DeltaId, context: DeltaIOContext)(implicit refAdder: RefAdder[A]): ModelAndDeltas[A] =
      copy(model = {
        val result = delta.runWith(model, context, deltaId)
        refAdder.addRefs(result)
      })

    def modelWithDeltas(implicit refAdder: RefAdder[A]): A = deltas.foldLeft(model) { case (m, d) => refAdder.addRefs(d.runWith(m)) }
  }

}

/**
  * State for a root component, with data needed by client to synchronise with server.
  *
  * @param id                 This client's id
  * @param nextClientDeltaId  The delta id we will use for the next new local delta
  * @param serverModel        The most recent model received from the server, and its id.
  * @param pendingDeltas      The deltas generated by this client since the most recent server
  *                           model, in the order they were generated and should be applied, with
  *                           their ids and contexts
  * @param model              The model currently displayed in the UI, incorporating pendingDeltas
  *                           applied in sequence to serverModel
  * @tparam A                 The type of model
  */
case class ClientState[A](id: ClientId, nextClientDeltaId: ClientDeltaId, serverModel: ModelAndId[A], pendingDeltas: Seq[DeltaWithIC[A]], model: A) {

  import ClientState.ModelAndDeltas

  /**
    * Apply a new delta to this client state, to produce a new client state
    * @param delta  The delta to apply
    * @param context The context in which to apply the delta. This is locally generated on the client,
    *                and will be replaced with a server-generated context when it reaches the server.
    *                We will then get a server update with the server context, which we use to rerun
    *                the delta appropriately.
    * @return A new ClientState
    */
  def apply(delta: Delta[A], context: DeltaIOContext)(implicit refAdder: RefAdder[A]): (ClientState[A], DeltaId) = {
    val deltaId = DeltaId(id, nextClientDeltaId)

    val nextModel = refAdder.addRefs(delta.runWith(model, context, deltaId))

    val deltaWithIC = DeltaWithIC(delta, deltaId, context)
    val state = copy(nextClientDeltaId = nextClientDeltaId.next, model = nextModel, pendingDeltas = pendingDeltas :+ deltaWithIC)
    (state, deltaId)
  }

  /**
    * Reconcile a ModelUpdate to this client state, to produce a new client state.
    * @param update The update to apply
    * @return An error xor a new ClientState
    */
  def update(update: ModelUpdate[A])(implicit idGen: ModelIdGen[A], refAdder: RefAdder[A]): Either[String, ClientState[A]] = update match {
    case i@ModelIncrementalUpdate(_,_,_) => incrementalUpdate(i)
    case f@ModelFullUpdate(_,_)  => fullUpdate(f)
  }

  /**
    * Reconcile a full update to this client state, to produce a new client state. This will clear all pendingDeltas
    * and set serverModel and model to the provided full model, and use new client id.
    * @param update The update to apply
    * @return A new ClientState
    */
  def fullUpdate(update: ModelFullUpdate[A])(implicit idGen: ModelIdGen[A]): Either[String, ClientState[A]] = ClientState.fromFullUpdate(update, nextClientDeltaId)

  /**
    * Reconcile an incremental update to this client state, to produce a new client state, if possible.
    * @param update The update to apply
    * @return An error xor a new ClientState
    */
  def incrementalUpdate(update: ModelIncrementalUpdate[A])(implicit idGen: ModelIdGen[A], refAdder: RefAdder[A]): Either[String, ClientState[A]] = {
    if (update.baseModelId != serverModel.id) {
      Left("Server update expected base model id " + update.baseModelId + " but we have server model id " + serverModel.id)

    } else {

      //Convert the updates into real deltas, requiring that all the LocalDeltas are actually in our pendingDeltas, in the
      //same order. As we do this, produce a new pendingDeltas with all local deltas up to and including the last LocalDelta
      //removed. The logic behind this is that:
      // 1. We require that the server applies our local deltas in the same order we did, although it is is fine for remote deltas
      //    to be interleaved.
      // 2. The server may omit applying local deltas, and if it does, we will discard them too
      //    (this might not be the best approach? We could instead require a new update type that rejects a local delta,
      //    probably with a message explaining why.)
      // 3. We can't apply a LocalDelta unless we know what it is - the server saves bandwidth by just referencing them by id.
      // 4. We keep hold of any pendingDeltas AFTER the most recent LocalDelta since the server may not have received them yet, so
      //    we need to retain them until they are either applied or we see that they are rejected because they are skipped
      //    (or explicitly rejected if implemented in future).
      // 5. The reduced pending delta list can then be used to build our new client model, optimistically applying deltas we
      //    believe are still pending (on their way to the server and back).

      val initialMAD: Either[String, ModelAndDeltas[A]] = Right(ModelAndDeltas(serverModel.model, pendingDeltas))

      val updatedMAD = update.deltas.foldLeft(initialMAD){
        case (Right(mad), u) => u match {
          //For remote deltas, just apply them to the model, pending deltas are unaffected
          case RemoteDelta(delta, remoteId, context) => Right(mad.applyDelta(delta, remoteId, context))

          // For local deltas, find them in pendingDeltas.
          // We get a new context to run them, which may give different results
          // to the context we produced on the client (notably, server time when executed is probably
          // different to client time)
          case LocalDelta(localDeltaId, context) =>

            //Drop all deltas before the matching local delta (or drop everything if there is no matching local delta)
            val remainingDeltas = mad.deltas.dropWhile(_.id != localDeltaId)
            remainingDeltas match {
              case delta +: tail => Right(
                mad.copy(
                  // Make sure to use the new context from server!
                  model = refAdder.addRefs(delta.runWithNewContext(mad.model, context)),
                  deltas = tail
                )
              )

              case Nil => Left("No pending delta for server-specified local id " + localDeltaId )
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
            Left("Locally-generated id " + genId + " does not match remote-generated id " + update.updatedModelId)
          // Missing id or matched id give success
          case _ => Right(mad)
        }
      // Produce updated client state from the checked model and deltas - new model
      // has remaining deltas applied, server model doesn't, pending deltas are those remaining in checkedMAD
      } yield copy(model = checkedMad.modelWithDeltas, serverModel = ModelAndId(checkedMad.model, update.updatedModelId), pendingDeltas = checkedMad.deltas)

    }
  }
}