package org.rebeam.tree.ot

/**
  * An Action contains the response to a client or server message,
  * which always updates the client state, and optionally also
  * requires an update to the client's document or the sending
  * of a client message to the server.
  * @tparam A     The type of element in the document list
  */
sealed trait Action[A] {
  /**
    * The new state of the client
    */
  val state: ClientState[A]
}

object Action {
  /**
    * Update client state, and send a message to the server with
    * an operation requested by the client
    * @param op     The operation to send to the server
    * @param state  The new client state
    * @tparam A     The type of element in the document list
    */
  case class Send[A](op: OpRev[A], state: ClientState[A]) extends Action[A]

  /**
    * Update client state, and apply an operation received from the server
    * to the current client document
    * @param op     The operation to apply to the client document
    * @param state  The new client state
    * @tparam A     The type of element in the document list
    */
  case class Apply[A](op: OpRev[A], state: ClientState[A]) extends Action[A]

  /**
    * Just update the client state
    * @param state  The new client state
    * @tparam A     The type of element in the document list
    */
  case class Shift[A](state: ClientState[A]) extends Action[A]
}

import Action._

sealed trait ClientState[A] {
//
//  /**
//    * The document revision we are synchronised at
//    */
//  val rev: Int
//
//  /**
//    * Produce an Action that will update the client state with the
//    * results of an operation requested by the client (i.e. a local edit)
//    * @param op The client operation
//    * @return   Resulting Action
//    */
//  def applyClient(op: OpRev[A]): Action[A]
//  def applyServer(op: OpRev[A]): Action[A]
//}
//
//object ClientState {
//
//  /**
//    * Client is synchronised with server - it has had no client edits since the most
//    * recent update from the server (applyServer)
//    * @param rev  The document revision we are synchronised at
//    * @tparam A   The type of element in the document list
//    */
//  case class Synchronized[A](rev: Int) extends ClientState[A] {
//    def applyClient(op: OpRev[A]): Action[A] = {
//      val op2 = op.reparent(version)
//      Send(op2, AwaitingConfirm(op2, version))
//    }
//
//    def applyServer(op: OpRev[A]): Action[A] = Apply(op, Synchronized(op.version))
//  }
//
//  case class AwaitingConfirm[A](outstanding: OpRev[A], version: Int) extends ClientState[A] {
//    def applyClient(op: OpRev[A]): Action[A] =
//      Shift(AwaitingWithBuffer(outstanding, op.reparent(outstanding.version), version))
//
//    def applyServer(op: OpRev[A]): Action[A] = {
//      if (op.id == outstanding.id) {
//        Shift(Synchronized(op.version))
//      } else {
//        val pair = Transformer.transform(outstanding.delta, op.delta)
//        val (client, server) = (pair.clientOp, pair.serverOp)
//        val outstanding2 = outstanding.copy(delta = client)
//        Apply(op.copy(delta = server), AwaitingConfirm(outstanding2.reparent(op.version), op.version))
//      }
//    }
//  }
//
//  case class AwaitingWithBuffer[A](outstanding: OpRev[A], buffer: OpRev[A], version: Int) extends ClientState[A] {
//    def applyClient(op: OpRev[A]): Action[A] = {
//      val buffer2 = buffer.copy(id = op.id, version = buffer.version + 1, delta = Composer.compose(buffer.delta, op.delta))
//      Shift(AwaitingWithBuffer(outstanding, buffer2, version))
//    }
//
//    def applyServer(op: OpRev[A]): Action[A] = {
//      if (op.id == outstanding.id) {
//        Send(buffer, AwaitingConfirm(buffer, op.version))
//      } else {
//        val pair = Transformer.transform(outstanding.delta, op.delta)
//        val (client, server) = (pair.clientOp, pair.serverOp)
//        val outstanding2 = outstanding.copy(delta = client).reparent(op.version)
//
//        val pair2 = Transformer.transform(buffer.delta, server)
//        val (client2, server2) = (pair2.clientOp, pair2.serverOp)
//        val buffer2 = buffer.copy(delta = client2).reparent(outstanding2.version)
//
//        Apply(op.copy(delta = server2), AwaitingWithBuffer(outstanding2, buffer2, op.version))
//      }
//    }
//  }

}




