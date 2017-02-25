package org.rebeam.tree.server

import scalaz.concurrent.Task
import scalaz.stream.{Exchange, Process, Sink}
import org.rebeam.tree.server.util._

import scalaz.Scalaz.none
import org.http4s.websocket.WebsocketBits._
import org.rebeam.tree._
import io.circe._
import io.circe.parser._
import cats.syntax.either._

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
private class TreeStoreValueDispatcher[T](val store: TreeStore[T])(implicit encoder: Encoder[T], deltaDecoder: Decoder[Delta[T]]) extends Dispatcher[T, Json, Json] {

  var pendingModelToClient = none[T]

  //Store pending model
  override def modelUpdated(model: T): Unit = {
    pendingModelToClient = Some(model)
  }

  //Clear pending model. If we had a value, write it as a "value" delta
  //to completely replace model on client
  override def msgForClient(): Option[Json] = {
    val model = pendingModelToClient
    pendingModelToClient = None
    model.map(m => Json.obj("value" -> encoder(m)))
  }

  //Read the Js.Value as a delta, and apply it to the store
  override def msgFromClient(msg: Json): Unit = {
    val empty = msg.asObject.exists(_.fields.isEmpty)
    if (empty) {
      println("Pong!")
    } else {
      val delta = deltaDecoder.decodeJson(msg)
      println("Applying delta " + delta)
      println("  from json " + msg)
      //TODO a bit messy using map just for side effect?
      delta.map(store.applyDelta)
    }
  }
}

object TreeStoreValueExchange {
  def apply[M](store: TreeStore[M])(implicit encoder: Encoder[M], decoder: Decoder[M], deltaDecoder: Decoder[Delta[M]]): Exchange[WebSocketFrame, WebSocketFrame] = {

    val dispatcher = new TreeStoreValueDispatcher(store)
    val observer = new DispatchObserver(dispatcher)
    store.observe(observer)

    //Treat received text as JSON encoded deltas to data, if valid
    val sink: Sink[Task, WebSocketFrame] = Process.constant {
      case Text(t, _) => Task.delay( parse(t).toOption.foreach(msg => dispatcher.msgFromClient(msg)) )
    }

    //Get source of messages for client from the observer's process
    val source = observer.process.map(js => Text(js.noSpaces))

    Exchange(source, sink)
  }
}

