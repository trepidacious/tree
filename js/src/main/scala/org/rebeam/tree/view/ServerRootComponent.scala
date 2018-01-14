package org.rebeam.tree.view

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomElement
import org.rebeam.tree._
import org.rebeam.tree.sync.{ClientState, Guid, RefAdder, Ref => TreeRef}
import org.rebeam.tree.sync.Sync._
import org.scalajs.dom._

import scala.scalajs.js.timers._
import scala.util.{Failure, Success}
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import org.rebeam.tree.DeltaCodecs.DeltaCodec
import org.rebeam.tree.ref.{Mirror, MirrorAndId, MirrorCodec}

object ServerRootComponent {

  trait RootSource[R] {
    def rootFor(rootModel: R, parent: Parent[R]): Root
  }


  private class MirrorRootSource extends RootSource[Mirror] {
    def rootFor(rootModel: Mirror, parent: Parent[Mirror]): Root = new Root {
      def cursorAt[A, L](ref: TreeRef[A], location: L)
                        (implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, L]] = {
        rootModel.apply(ref).map { data =>
          Cursor[A, L](MirrorParent[A](parent, ref), data, location, this)
        }
      }
      def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid] = {
        refGuids.flatMap(refGuid => rootModel.revisionOf(refGuid).map(refGuid -> _)).toMap
      }
    }
  }

  implicit val mirrorRootSource: RootSource[Mirror] = new MirrorRootSource

  private class MirrorAndIdRootSource[M] extends RootSource[MirrorAndId[M]] {

    def rootFor(rootModel: MirrorAndId[M], parent: Parent[MirrorAndId[M]]): Root = new Root {
      def cursorAt[A, L](ref: TreeRef[A], location: L)
                        (implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, L]] = {
        rootModel.mirror.apply(ref).map { data =>
          val mirrorAndIdParent = MirrorAndIdParent[M](parent)
          Cursor[A, L](MirrorParent[A](mirrorAndIdParent, ref), data, location, this)
        }
      }
      def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid] = {
        refGuids.flatMap(refGuid => rootModel.mirror.revisionOf(refGuid).map(refGuid -> _)).toMap
      }
    }
  }

  implicit def mirrorAndIdRootSource[M]: RootSource[MirrorAndId[M]] = new MirrorAndIdRootSource

  private class NoRootSource[R] extends RootSource[R] {
    def rootFor(rootModel: R, parent: Parent[R]): Root = Cursor.RootNone
  }

  def noRootSource[R]: RootSource[R] = new NoRootSource[R]

  case class Props[R, P](p: P, render: Cursor[R, P] => VdomElement, wsUrl: String, noData: VdomElement)

  case class State[R](clientState: Option[ClientState[R]], ws: Option[WebSocket], tick: Option[SetIntervalHandle])

  class Backend[R, P](scope: BackendScope[Props[R, P], State[R]])
    (implicit decoder: Decoder[R], deltaDecoder: DeltaCodec[R], idGen: ModelIdGen[R], contextSource: DeltaIOContextSource, rootSource: RootSource[R], refAdder: RefAdder[R], searchable: Searchable[R, Guid]) {

    implicit val cme = clientMsgEncoder[R]

    val deltaToCallback: Delta[R] => Callback =
      (delta: Delta[R]) => for {
        s <- scope.state
        _ <- deltaDecoder.encoder(delta).fold(
            Callback {
              println(s"!!! Could not encode delta $delta")
            }
          )(
            deltaJs =>
              s.clientState match {
                case None =>
                  // TODO implement
                  Callback {
                    println("!!! Delta before we have a clientState! Should queue deltas?")
                  }
                case Some(cs) => {
                  //SIDE-EFFECT: Note this is the point at which we generate the context,
                  //and so read the time
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
        )
      } yield {}


    val rootParent = RootParent[R](deltaToCallback)

    def render(props: Props[R, P], state: State[R]) = {
      state.clientState.map { cs =>
        val cursorP: Cursor[R, P] = Cursor(rootParent, cs.model, props.p, rootSource.rootFor(cs.model, rootParent))
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
        // This means that calls like .setState will now return Unit instead of Callback.
        val direct = scope.withEffectsImpure

        // These are message-receiving events from the WebSocket "thread".

        def onOpen(e: Event): Unit = println("Connected.")

        def onMessage(e: MessageEvent): Unit = {
//          println(s"Updating with: ${e.data.toString}")
          val msg = e.data.toString

          parse(msg).fold[Unit](
            pf => println("Invalid JSON from server " + pf),
            json => updateDecoder[R].decodeJson(json).fold(
              df => println("Could not decode JSON from server " + df + ":\n" + msg),
              update => {

                val newCSX = direct.state.clientState.fold(
                  ClientState.fromFirstUpdate[R](update)
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

  def factory[R, P]
    (noData: VdomElement, wsUrl: String)
    (render: Cursor[R, P] => VdomElement)
    (implicit decoder: Decoder[R], deltaCodec: DeltaCodec[R], idGen: ModelIdGen[R], contextSource: DeltaIOContextSource, rootSource: RootSource[R], refAdder: RefAdder[R], searchable: Searchable[R, Guid]) = {
    val c = ctor[R, P](decoder, deltaCodec, idGen, contextSource, rootSource, refAdder, searchable)
    (page: P) => c(Props[R, P](page, render, wsUrl, noData))
  }


  def apply[R]
  (noData: VdomElement, wsUrl: String)
  (render: Cursor[R, Unit] => VdomElement)
  (implicit decoder: Decoder[R], deltaCodec: DeltaCodec[R], idGen: ModelIdGen[R], contextSource: DeltaIOContextSource, rootSource: RootSource[R], refAdder: RefAdder[R], searchable: Searchable[R, Guid]) = {
    val c = ctor[R, Unit](decoder, deltaCodec, idGen, contextSource, rootSource, refAdder, searchable)
    c(Props[R, Unit]((), render, wsUrl, noData))
  }

  //Just make the component constructor - props to be supplied later to make a component
  def ctor[R, P](implicit decoder: Decoder[R], deltaCodec: DeltaCodec[R], idGen: ModelIdGen[R], contextSource: DeltaIOContextSource, rootSource: RootSource[R], refAdder: RefAdder[R], searchable: Searchable[R, Guid]) = ScalaComponent.builder[Props[R, P]]("ServerRootComponent")
    .initialState(State[R](None, None, None))
    .backend(new Backend[R, P](_)(decoder, deltaCodec, idGen, contextSource, rootSource, refAdder, searchable))
    .render(s => s.backend.render(s.props, s.state))
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

}
