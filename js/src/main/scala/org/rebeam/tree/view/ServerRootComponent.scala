package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import org.rebeam.tree.sync.ClientState
import org.rebeam.tree.sync.Sync._
import org.scalajs.dom._

import scala.scalajs.js.timers._
import scala.util.{Failure, Success}
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.rebeam.tree.DeltaCodecs.DeltaCodec

object ServerRootComponent {

  case class Props[A, C, L](location: L, render: Cursor[A, C, L] => ReactElement, wsUrl: String, noData: ReactElement)

  case class State[A](clientState: Option[ClientState[A]], ws: Option[WebSocket], tick: Option[SetIntervalHandle])

  class Backend[A, C, L](scope: BackendScope[Props[A, C, L], State[A]])
    (implicit decoder: Decoder[A], aCodec: DeltaCodec[A], cCodec: DeltaCodec[C], idGen: ModelIdGen[A], contextSource: DeltaIOContextSource) {

    implicit val cme = clientMsgEncoder[A]

    val deltaToCallback: (Delta[A], Json) => Callback =
      (delta: Delta[A], deltaJs: Json) =>
        for {
          s <- scope.state
          _ <- s.clientState match {
            case None =>
              // TODO implement
              Callback{"Delta before we have a clientState! Should queue deltas?"}
            case Some(cs) => {
              //SIDE-EFFECT: Note this is the point at which we generate the context
              val (newCS, id) = cs.apply(delta, contextSource.getContext)
              val dij = DeltaWithIJ(delta, id, deltaJs)
              for {
                _ <- scope.setState(s.copy(clientState = Some(newCS)))
                // TODO should store up deltas if we don't have a websocket, and send when we do
                _ <- Callback {
                  val msg = dij.asJson.noSpaces
                  s.ws.foreach(socket => socket.send(msg))
                }
              } yield {}
            }
          }
        } yield {}

    private val rootParent = RootParent[A](deltaToCallback)

    def render(props: Props[A, C, L], state: State[A]) = {
      state.clientState.map { cs =>
        val cursorP = Cursor(rootParent, cs.model, props.location, Cursor.emptyCacheVAP[C]) //FIXME actually use the cache
        props.render(cursorP)
      }.getOrElse(
        props.noData
      )
    }

    def start: Callback = {

      // This will establish the connection and return the WebSocket
      def connect(u: String) = CallbackTo[(WebSocket, SetIntervalHandle)] {

        val url = if (!u.startsWith("ws:")) {
          "ws://" + document.location.hostname + ":" + document.location.port + "/" + u
        } else {
          u
        }


        // Get direct access so WebSockets API can modify state directly
        // (for access outside of a normal DOM/React callback).
        val direct = scope.accessDirect

        // These are message-receiving events from the WebSocket "thread".

        def onOpen(e: Event): Unit = println("Connected.")

        def onMessage(e: MessageEvent): Unit = {
//          println(s"Updating with: ${e.data.toString}")
          val msg = e.data.toString

          parse(msg).fold[Unit](
            pf => println("Invalid JSON from server " + pf),
            json => updateDecoder[A].decodeJson(json).fold(
              df => println("Could not decode JSON from server " + df + ":\n" + msg),
              update => {

                val newCSX = direct.state.clientState.fold(
                  ClientState.fromFirstUpdate[A](update)
                )(
                  _.update(update)
                )

                newCSX.fold(
                  error => println("Can't use server update: " + error),
                  newCS => direct.modState(_.copy(clientState = Some(newCS)))
                )

              }
            )
          )
        }

        def onError(e: ErrorEvent): Unit = {
          // TODO recover?
          println(s"Error: ${e.message}")
        }

        def onClose(e: CloseEvent): Unit = {
          // TODO reconnect
          println(s"Closed: ${e.reason}")
          // Close the connection
          direct.modState(_.copy(ws = None))
        }

        // Create WebSocket and setup listeners
        val ws = new WebSocket(url)
        ws.onopen = onOpen _
        ws.onclose = onClose _
        ws.onmessage = onMessage _
        ws.onerror = onError _

        //Regularly send ping (empty object) through websocket, if we have one
        val tick = setInterval(5000) {
          println("Ping!")
          direct.state.ws.foreach(_.send("{}"))
        }

        (ws, tick)
      }

      // Here use attemptTry to catch any exceptions in connect.
      scope.props.map(_.wsUrl).flatMap(connect).attemptTry.flatMap {
        case Success((ws, tick)) => scope.modState(_.copy(ws = Some(ws)).copy(tick = Some(tick)))
        // TODO handle failure
        case Failure(error) => Callback(println(error.toString))
      }
    }

    def end: Callback = {
      def closeWebSocket = scope.state.map(_.ws.foreach(_.close()))
      def clearWebSocket = scope.modState(_.copy(ws = None))
      def clearTick = scope.state.map(_.tick.foreach(clearInterval)) >> scope.modState(_.copy(tick = None))
      clearTick >> closeWebSocket >> clearWebSocket
    }

  }

  def factory[A, C, L]
    (noData: ReactElement, wsUrl: String)
    (render: Cursor[A, C, L] => ReactElement)
    (implicit decoder: Decoder[A], aCodec: DeltaCodec[A], cCodec: DeltaCodec[C], idGen: ModelIdGen[A], contextSource: DeltaIOContextSource) = {
    val c = ctor[A, C, L](decoder, aCodec, cCodec, idGen, contextSource)
    (location: L) => c(Props[A, C, L](location, render, wsUrl, noData))
  }


  def apply[A]
  (noData: ReactElement, wsUrl: String)
  (render: Cursor[A, Unit, Unit] => ReactElement)
  (implicit decoder: Decoder[A], aCodec: DeltaCodec[A], idGen: ModelIdGen[A], contextSource: DeltaIOContextSource) = {
    val c = ctor[A, Unit, Unit](decoder, aCodec, DeltaCodecs.unitDeltaCodec, idGen, contextSource)
    //val cpRender = (cp: Cursor[A, Unit, Unit]) => render.apply(cp)
    c(Props[A, Unit, Unit]((), render, wsUrl, noData))
  }

  //Just make the component constructor - props to be supplied later to make a component
  def ctor[A, C, L](implicit decoder: Decoder[A], aCodec: DeltaCodec[A], cCodec: DeltaCodec[C], idGen: ModelIdGen[A], contextSource: DeltaIOContextSource) = ReactComponentB[Props[A, C, L]]("ServerRootComponent")
    .initialState(State[A](None, None, None))
    .backend(new Backend[A, C, L](_)(decoder, aCodec, cCodec, idGen, contextSource))
    .render(s => s.backend.render(s.props, s.state))
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

}
