package org.rebeam.tree.server

import org.rebeam.tree.{Delta, DeltaIOContext, DeltaIOContextSource}
import org.rebeam.tree.server.util._
import org.rebeam.tree.sync.{DeltaIORun, ServerStoreUpdate}
import org.rebeam.tree.sync.ServerStoreUpdate._
import org.rebeam.tree.sync.Sync._
import DeltaIORun._

import scalaz.Scalaz._
import org.http4s.websocket.WebsocketBits.WebSocketFrame

import scalaz.concurrent.Task
import scalaz.stream.{Exchange, Process, Sink}
import org.http4s.websocket.WebsocketBits._
import io.circe._
import io.circe.parser._
import cats.syntax.either._
import org.rebeam.tree.DeltaCodecs.DeltaCodec

/**
  * Stores a model, and allows that model to be updated (mutated) by
  * providing ServerDeltas, assigning ModelIds to . Can be observed to receive a firstUpdate with
  * the model
  * @param initialModel   The initial model for the store
  * @tparam A             The type of model in the store
  */
class ServerStore[A: ModelIdGen](initialModel: A) {

  private var nextId: Long = 0
  private var m: ModelAndId[A] = ModelAndId(initialModel, makeId(initialModel))
  private val lock = Lock()
  private val observers = new WeakHashSet[Observer[ServerStoreUpdate[A]]]()

  private def makeId(model: A): ModelId = {
    val mId = implicitly[ModelIdGen[A]].genId(initialModel).getOrElse(ModelId(nextId))
    nextId = nextId + 1
    mId
  }

  def applyDelta(d: DeltaWithIJ[A], context: DeltaIOContext): Unit = lock {
    val baseModelId = m.id

    val newModel = d.runWith(m.model, context)

    val newId = makeId(newModel)
    m = ModelAndId(newModel, newId)
    observers.foreach(_.observe(ServerStoreIncrementalUpdate(baseModelId, Vector(d.withContext(context)), newId)))
  }

  def observe(o: Observer[ServerStoreUpdate[A]]): Unit = lock {
    observers.add(o)
    o.observe(ServerStoreFullUpdate(m))
  }

  def unobserve(o: Observer[ServerStoreUpdate[A]]): Unit = lock{
    observers.remove(o)
  }

}

/**
  * Uses DeltaWithIJs encoded as JSON for incoming messages, and ModelUpdates encoded to JSON for
  * outgoing messages.
  */
private class ServerStoreValueDispatcher[T](val store: ServerStore[T], val clientId: ClientId, contextSource: DeltaIOContextSource)(implicit encoder: Encoder[T], deltaDecoder: DeltaCodec[T]) extends Dispatcher[ServerStoreUpdate[T], Json, Json] {

  private var pendingUpdateToClient = none[ServerStoreUpdate[T]]

  private val updateEncoder = serverStoreUpdateEncoder[T](clientId)

  //Store pending update
  override def modelUpdated(update: ServerStoreUpdate[T]): Unit = {
//    println("New update for client id " + clientId.id + ": " + update)

    pendingUpdateToClient = Some(pendingUpdateToClient.fold(update)(
      e => e.append(update)
    ))
  }

  //Clear pending update. If we had a value, emit it as JSON
  override def msgForClient(): Option[Json] = {
    val update = pendingUpdateToClient
//    println("Sending update to client id " + clientId.id + ": " + update)
    pendingUpdateToClient = None
    update.map(updateEncoder(_))
  }

  //Read the Js.Value as a delta, and apply it to the store
  override def msgFromClient(msg: Json): Unit = {
    val empty = msg.asObject.exists(_.fields.isEmpty)
    if (empty) {
//      println("Pong!")
    } else {

      val deltaWithIJ = clientMsgDecoder.decodeJson(msg)

      deltaWithIJ.fold (
        error => println("Error decoding client message: " + error + "\nMessage: " + msg),
        //Reject deltas not from expected client
        dij => if (dij.id.clientId != clientId) {
          println("Got clientId " + dij.id.clientId + ", expected " + clientId)
        } else {
//          println("Committing msg from client id " + clientId.id + ": " + dij)
          store.applyDelta(dij, contextSource.getContext)
        }
      )
    }
  }
}

object ServerStoreValueExchange {
  def apply[M](store: ServerStore[M], clientId: ClientId, contextSource: DeltaIOContextSource)(implicit encoder: Encoder[M], decoder: Decoder[M], deltaDecoder: DeltaCodec[M]): Exchange[WebSocketFrame, WebSocketFrame] = {

    val dispatcher = new ServerStoreValueDispatcher(store, clientId, contextSource)
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

