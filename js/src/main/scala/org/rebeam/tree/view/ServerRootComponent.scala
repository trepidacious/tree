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

object ServerRootComponent {

  case class Props[R](render: Cursor[R] => ReactElement, wsUrl: String, noData: ReactElement)

  case class State[R](clientState: Option[ClientState[R]], ws: Option[WebSocket], tick: Option[SetIntervalHandle])

  class Backend[R](scope: BackendScope[Props[R], State[R]])(implicit decoder: Decoder[R], deltaDecoder: Decoder[Delta[R]]) {

    def encodeDeltaWithIj(id: DeltaId, js: Json) = Json.obj(
      "commit" -> Json.obj(
        "delta" -> js,
        "id" -> id.asJson
      )
    )

    val deltaToCallback: (Delta[R], Json) => Callback =
      (delta: Delta[R], deltaJs: Json) =>
        for {
          s <- scope.state
          _ <- s.clientState match {
            case None =>
              Callback{"Delta before we have a clientState! Should queue deltas?"}
            case Some(cs) => {
              val (newCS, id) = cs.apply(delta)
              for {
                _ <- scope.setState(s.copy(clientState = Some(newCS)))
                _ <- Callback(s.ws.foreach(socket => socket.send(encodeDeltaWithIj(id, deltaJs).toString)))
              } yield {}
            }
          }
        } yield {}

    val rootParent = RootParent[R](deltaToCallback)

    def render(props: Props[R], state: State[R]) = {
      state.clientState.map { cs =>
        val rootCursor = org.rebeam.tree.view.Cursor[R](rootParent, cs.model)
        props.render(rootCursor)
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

        def onopen(e: Event): Unit = println("Connected.")

        def onmessage(e: MessageEvent): Unit = {
          println(s"Updating with: ${e.data.toString}")
          val msg = e.data.toString
          parse(msg).fold[Unit](
            pf => console.log("Invalid JSON from server " + pf),
            json => valueDecoder.decodeJson(json).fold(
              df => console.log("Could not decide JSON from server " + df),
              m => direct.modState(_.copy(model = Some(m)))
            )
          )
        }

        def onerror(e: ErrorEvent): Unit = println(s"Error: ${e.message}")

        def onclose(e: CloseEvent): Unit = {
          println(s"Closed: ${e.reason}")
          // Close the connection
          direct.modState(_.copy(ws = None))
        }

        // Create WebSocket and setup listeners
        val ws = new WebSocket(url)
        ws.onopen = onopen _
        ws.onclose = onclose _
        ws.onmessage = onmessage _
        ws.onerror = onerror _

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

  //Make the component itself, by providing a render method to initialise the props
  def apply[R](noData: ReactElement, wsUrl: String)
              (render: Cursor[R] => ReactElement)(implicit decoder: Decoder[R], deltaDecoder: Decoder[Delta[R]]) =
    ctor(decoder, deltaDecoder)(Props[R](render, wsUrl, noData))

  //Just make the component constructor - props to be supplied later to make a component
  def ctor[R](implicit decoder: Decoder[R], deltaDecoder: Decoder[Delta[R]]) = ReactComponentB[Props[R]]("TreeRootComponent")
    .initialState(State[R](None, None, None))
    .backend(new Backend[R](_)(decoder, deltaDecoder))
    .render(s => s.backend.render(s.props, s.state))
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

}
