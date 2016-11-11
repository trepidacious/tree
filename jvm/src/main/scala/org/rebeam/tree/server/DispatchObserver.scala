package org.rebeam.tree.server

import org.rebeam.tree.server.util.Lock

import scalaz.concurrent.Task
import scalaz.{\/, \/-}
import scalaz.Scalaz.none
import scalaz.stream.{Exchange, Process, Sink}

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
