package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree.Delta
import org.scalajs.dom._
import upickle.Js

import scala.util.{Failure, Success}

object TreeRootComponent {

  case class Props[R](render: Cursor[R] => ReactElement, wsUrl: String)
  case class State[R](model: R, ws: Option[WebSocket])

  class Backend[R](scope: BackendScope[Props[R], State[R]]) {

    //Apply the delta, and print its Json. In a real implementation this
    //would still apply the delta, but would also send the Json to a server
    //to attempt to "commit" the change. The state might actually store a
    //tentative model as modified locally, and a last-known authoritative
    //model from the server, to allow reverting local modifications if they
    //are not confirmed, or merging them if the server reports it merged them.
    val deltaToCallback = (delta: Delta[R], deltaJs: Js.Value) => {
      val applyDelta = scope.modState(s => s.copy(model = delta.apply(s.model)))
      val sendDeltaJs = scope.state.flatMap(s => Callback(s.ws.foreach(_.send(deltaJs.toString()))))
      applyDelta >> sendDeltaJs
    }

    val rootParent = RootParent(deltaToCallback)

    def render(props: Props[R], state: State[R]) = {
      val rootCursor = Cursor(rootParent, state.model)
      props.render(rootCursor)
    }

    def start: Callback = {

      // This will establish the connection and return the WebSocket
      def connect(url: String) = CallbackTo[WebSocket] {

        // Get direct access so WebSockets API can modify state directly
        // (for access outside of a normal DOM/React callback).
        val direct = scope.accessDirect

        // These are message-receiving events from the WebSocket "thread".

        def onopen(e: Event): Unit = println("Connected.")

        def onmessage(e: MessageEvent): Unit = println(s"Echo: ${e.data.toString}")

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

  //TODO get rid of the initial model, and make model an Option[R] so we can start
  //empty and get data from WS while displaying a notice that data is loading
  //Make the component itself, by providing a render method to initialise the props
  def apply[R](initialModel: R, wsUrl: String)
              (render: Cursor[R] => ReactElement) =
    ctor(initialModel)(Props[R](render, wsUrl))

  //Just make the component constructor - props to be supplied later to make a component
  def ctor[R](initialModel: R) = ReactComponentB[Props[R]]("TreeRootComponent")
    .initialState(State(initialModel, None))
    .backend(new Backend[R](_))
    .render(s => s.backend.render(s.props, s.state))
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

}
