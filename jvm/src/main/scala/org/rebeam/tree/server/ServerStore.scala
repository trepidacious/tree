package org.rebeam.tree.server

import org.rebeam.tree.{Delta, DeltaIOContext, DeltaIOContextSource}
import org.rebeam.tree.server.util._
import org.rebeam.tree.sync.{DeltaIORun, ServerStoreUpdate}
import org.rebeam.tree.sync.ServerStoreUpdate._
import org.rebeam.tree.sync.Sync._
import org.rebeam.tree.sync.RefAdder
import DeltaIORun._

import scalaz.Scalaz._
import org.http4s.websocket.WebsocketBits.WebSocketFrame

import scalaz.concurrent.Task
import scalaz.stream.{Exchange, Process, Sink}
import org.http4s.websocket.WebsocketBits._
import io.circe._
import io.circe.parser._
import cats.syntax.either._

/**
  * Stores a model, and allows that model to be updated (mutated) by
  * providing ServerDeltas, assigning ModelIds to . Can be observed to receive a firstUpdate with
  * the model
  * @param initialModel   The initial model for the store
  * @tparam A             The type of model in the store
  * @tparam D             The type of delta on the model
  */
class ServerStore[A: ModelIdGen, D <: Delta[A]](initialModel: A) {

  private var nextId: Long = 0
  private var m: ModelAndId[A] = ModelAndId(initialModel, makeId(initialModel))
  private val lock = Lock()
  private val observers = new WeakHashSet[Observer[ServerStoreUpdate[A, D]]]()

  private def makeId(model: A): ModelId = {
    val mId = implicitly[ModelIdGen[A]].genId(initialModel).getOrElse(ModelId(nextId))
    nextId = nextId + 1
    mId
  }

  def applyDelta(d: DeltaAndId[A, D], context: DeltaIOContext)(implicit refAdder: RefAdder[A]): Unit = lock {
    val baseModelId = m.id

    val result = d.runWith(m.model, context)

    //Use RefAdder to actually add the refs to our model (if possible)
    val newModel = refAdder.addRefs(result)

    val newId = makeId(newModel)
    m = ModelAndId(newModel, newId)
    observers.foreach(_.observe(ServerStoreIncrementalUpdate(baseModelId, Vector(d.withContext(context)), newId)))
  }

  def observe(o: Observer[ServerStoreUpdate[A, D]]): Unit = lock {
    observers.add(o)
    o.observe(ServerStoreFullUpdate(m))
  }

  def unobserve(o: Observer[ServerStoreUpdate[A, D]]): Unit = lock{
    observers.remove(o)
  }

}

/**
  * Uses DeltaWithIJs encoded as JSON for incoming messages, and ModelUpdates encoded to JSON for
  * outgoing messages.
  */
private class ServerStoreValueDispatcher[T, D <: Delta[T]]
  (val store: ServerStore[T, D], val clientId: ClientId, contextSource: DeltaIOContextSource)
  (implicit encoder: Encoder[T], deltaDecoder: Decoder[D], deltaEncoder: Encoder[D], refAdder: RefAdder[T])
  extends Dispatcher[ServerStoreUpdate[T, D], Json, Json] {

  private var pendingUpdateToClient = none[ServerStoreUpdate[T, D]]

  private val updateEncoder = serverStoreUpdateEncoder[T, D](clientId)

  //Store pending update
  override def modelUpdated(update: ServerStoreUpdate[T, D]): Unit = {
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

      val deltaAndId = clientMsgDecoder[T, D].decodeJson(msg)

      deltaAndId.fold (
        error => println("Error decoding client message: " + error + "\nMessage: " + msg),
        //Reject deltas not from expected client
        dId => if (dId.id.clientId != clientId) {
          println("Got clientId " + dId.id.clientId + ", expected " + clientId)
        } else {
//          println("Committing msg from client id " + clientId.id + ": " + dij)
          store.applyDelta(dId, contextSource.getContext)
        }
      )
    }
  }
}

object ServerStoreValueExchange {
  def apply[M, D <: Delta[M]]
    (store: ServerStore[M, D], clientId: ClientId, contextSource: DeltaIOContextSource)
    (implicit encoder: Encoder[M], decoder: Decoder[M], deltaDecoder: Decoder[D], deltaEncoder: Encoder[D], refAdder: RefAdder[M]): Exchange[WebSocketFrame, WebSocketFrame] = {

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

