package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import org.scalajs.dom._

import io.circe._
import io.circe.parser._

import scala.util.{Failure, Success}

object WSRootComponent {

  case class Props[R](render: Cursor[R] => ReactElement, wsUrl: String, noData: ReactElement)
  case class State[R](model: Option[R], ws: Option[WebSocket])

  class Backend[R](scope: BackendScope[Props[R], State[R]])(implicit decoder: Decoder[R]) {

    //TODO decode incoming json as delta on R, so we can support non-value deltas
    private val valueDecoder: Decoder[R] = Decoder.instance(c =>c.downField("value").as[R])

    //Apply the delta, and print its Json. In a real implementation this
    //would still apply the delta, but would also send the Json to a server
    //to attempt to "commit" the change. The state might actually store a
    //tentative model as modified locally, and a last-known authoritative
    //model from the server, to allow reverting local modifications if they
    //are not confirmed, or merging them if the server reports it merged them.
    val deltaToCallback = (delta: Delta[R], deltaJs: Json) => {
      val applyDelta = scope.modState(s => s.copy(model = s.model.map(m => delta.apply(m))))
      val sendDeltaJs = scope.state.flatMap(s => Callback(s.ws.foreach(_.send(deltaJs.toString))))
      applyDelta >> sendDeltaJs
    }

    val rootParent = RootParent(deltaToCallback)

    def render(props: Props[R], state: State[R]) = {
      state.model.map { m =>
        val rootCursor = org.rebeam.tree.view.Cursor(rootParent, m)
        props.render(rootCursor)
      }.getOrElse(
        props.noData
      )
    }

    def start: Callback = {

      // This will establish the connection and return the WebSocket
      def connect(u: String) = CallbackTo[WebSocket] {

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
            json => valueDecoder.decodeJson(_).fold(
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
        ws
      }

      // Here use attemptTry to catch any exceptions in connect.
      scope.props.map(_.wsUrl).flatMap(connect).attemptTry.flatMap {
        case Success(ws) => scope.modState(_.copy(ws = Some(ws)))
        case Failure(error) => Callback(println(error.toString))
      }
    }

    def end: Callback = {
      def closeWebSocket = scope.state.map(_.ws.foreach(_.close()))
      def clearWebSocket = scope.modState(_.copy(ws = None))
      closeWebSocket >> clearWebSocket
    }

  }

  //Make the component itself, by providing a render method to initialise the props
  def apply[R](noData: ReactElement, wsUrl: String)
              (render: Cursor[R] => ReactElement)(implicit decoder: Decoder[R]) =
    ctor(decoder)(Props[R](render, wsUrl, noData))

  //Just make the component constructor - props to be supplied later to make a component
  def ctor[R](implicit decoder: Decoder[R]) = ReactComponentB[Props[R]]("TreeRootComponent")
    .initialState(State[R](None: Option[R], None: Option[WebSocket]))
    .backend(new Backend[R](_)(decoder))
    .render(s => s.backend.render(s.props, s.state))
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

}
