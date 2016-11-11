package org.rebeam.tree.server

/**
  * Handles dispatching between three streams of data, in
  * order to synchronise a single client with a data model.
  * The streams are:
  * 1. Deltas to a model. These are accepted as a steam of D instances,
  * and contain enough information to fully monitor a model. For example
  * in a simple case these could be just new entire models as immutable
  * data. In a more complex case this could be a stream of "key frames"
  * giving the entire model, and "delta frames" giving changes to it, where
  * we obviously expect to start with a key frame.
  * 2. Incoming messages from the client, generally containing requests from
  * that client to modify the model.
  * 3. Outgoing messages that should be supplied to the client to update it.
  *
  * The dispatcher is not thread safe, and must be called from only one thread
  * at any given time - for example use a Lock and apply it around the calls
  * to each method. The Dispatcher can then be written to expect a serialised
  * stream of calls to its three methods.
  *
  * @tparam D The type of delta to the model
  * @tparam I The type of message from the client
  * @tparam O The type of message to the client
  */
trait Dispatcher[D, I, O]  {

  /**
    * Called when there is a new delta to the model available
    */
  def modelUpdated(delta: D): Unit

  /**
    * Called to check for new message to send to client. This
    * has the side-effect of removing any provided message from
    * the "queue" - it is up to the user of this Dispatcher
    * to ensure that the message will then reach the client.
    */
  def msgForClient(): Option[O]

  /**
    * Called when a message is received from the client.
    */
  def msgFromClient(msg: I): Unit
}
