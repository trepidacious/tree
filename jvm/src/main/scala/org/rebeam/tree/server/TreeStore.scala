package org.rebeam.tree.server


import scalaz.concurrent.Task
import scalaz.stream.{Exchange, Process, Sink}
import org.rebeam.tree.server.util._

import scalaz.Scalaz.none

import org.http4s.websocket.WebsocketBits._
import org.rebeam.tree._
import upickle.Js
import upickle.default._

import scalaz.{\/, \/-}

//Observer performs a side effect whenever a data item of type T is produced
trait Observer[T] {
  def observe(t: T): Unit
}

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


/**
  * Uses an OutgoingDispatcher to provide Observer implementation.
  * Values provided to observe are passed on to an OutgoingDispatcher.
  * In addition, a pull method is provided. This accepts a callback, and
  * will provide data to that callback as soon as possible - if the
  * OutgoingDispatcher already has a message then it will be provided
  * immediately, otherwise the callback will be stored and as soon as
  * new data is observed it will be provided to the callback. These operations
  * are synchronised with the lock from the dispatcher.
  * In essence this provides a bridge between a callback to Observer that
  * receives updates to a model, a Process that provides messages to a client,
  * and a msgFromClient method that can accept messages from a client.
  *
  * @param d The dispatcher
  * @tparam D The type of delta to the model
  * @tparam I The type of message from the client
  * @tparam O The type of message to the client
  */
private class DispatchObserver[D, I, O](d: Dispatcher[D, I, O]) extends Observer[D] {

  var pendingPull = none[O => Unit]

  val lock = Lock()

  def observe(delta: D): Unit = lock {
    d.modelUpdated(delta)

    //If we have a pending pull and dispatcher now has a
    //message for client, give the message to the pull
    for {
      pull <- pendingPull
      msg <- d.msgForClient()
    } {
      //Clear this before calling pull, in case we get called back immediately
      pendingPull = None
      pull.apply(msg)
    }
  }

  def pull(pullF: O => Unit): Unit = lock {
    //Only one pull registered at a time
    if (pendingPull.isDefined) throw new RuntimeException (
      "DispatchObserver pull called when pull was already pending - " +
        "call pull again only when previous pull function is called back"
    )

    //Deliver data straight away if we have it, otherwise store the pull as pending
    d.msgForClient() match {
      case Some(s) => pullF.apply(s)
      case None => pendingPull = Some(pullF)
    }
  }

  //Process requires callback accepting Throwable \/ T, but we produce no Throwables, so provide
  //a wrapper that will always provide a T right value.
  def pullEither(pullE: (Throwable \/ O) => Unit): Unit = pull((o: O) => pullE(\/-(o)))

  //Produce a Process from this observer by wrapping the pullEither callback in the normal way.
  val process: Process[Task, O] = Process.repeatEval(Task.async { (cb: Throwable \/ O => Unit) => pullEither(cb)})

  //Incoming side - just pass through to Dispatcher, using our lock
  def msgFromClient(msg: I): Unit = lock {
    d.msgFromClient(msg)
  }

}

class TreeStore[T](initialModel: T) {
  private var m: T = initialModel
  private val lock = RWLock()
  private val observers = new WeakHashSet[Observer[T]]()

  def model: T = lock.read(m)
  def apply: T = model

  def applyDelta(d: Delta[T]): T = lock.write {
    m = d.apply(m)
    observers.foreach(_.observe(m))
    m
  }

  def observe(o: Observer[T]): Unit = lock.write {
    observers.add(o)
    o.observe(m)
  }

  def unobserve(o: Observer[T]): Unit = lock.write {
    observers.remove(o)
  }
}

/**
  * Uses Deltas encoded to JS.Value using a DeltaReader for incoming and outgoing messages,
  * and full updates to a model for deltas of that model. Always sends a complete change of
  * base model as outgoing messages.
  */
private class TreeStoreValueDispatcher[T: Writer: DeltaReader](val store: TreeStore[T]) extends Dispatcher[T, Js.Value, Js.Value] {

  var pendingModelToClient = none[T]

  //Store pending model
  override def modelUpdated(model: T): Unit = {
    pendingModelToClient = Some(model)
  }

  //Clear pending model. If we had a value, write it as a "value" delta
  //to completely replace model on client
  override def msgForClient(): Option[Js.Value] = {
    val model = pendingModelToClient
    pendingModelToClient = None
    model.map(m => Js.Obj("value" -> implicitly[Writer[T]].write(m)))
  }

  //Read the Js.Value as a delta, and apply it to the store
  override def msgFromClient(msg: Js.Value): Unit = {
//    println("TSVD got deltaJs " + msg + "...")
    val delta = implicitly[DeltaReader[T]].readDelta(msg)
    store.applyDelta(delta)
//    println(" applied, yields" + store.model)
  }
}

object TreeStoreValueExchange {
  def apply[M: Reader: Writer: DeltaReader](store: TreeStore[M]): Exchange[WebSocketFrame, WebSocketFrame] = {

    val dispatcher = new TreeStoreValueDispatcher(store)
    val observer = new DispatchObserver(dispatcher)

    //Treat received text as deltas to data
    val sink: Sink[Task, WebSocketFrame] = Process.constant {
      case Text(t, _) => Task.delay( dispatcher.msgFromClient(upickle.json.read(t)) )
    }

    //Get source of messages for client from the observer's process
    val source = observer.process.map(js => Text(upickle.json.write(js)))

    Exchange(source, sink)
  }
}

