package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import org.rebeam.tree.sync.{ClientState, Guid, RefAdder, Ref => TreeRef}
import org.rebeam.tree.sync.Sync._
import org.scalajs.dom._

import scala.scalajs.js.timers._
import scala.util.{Failure, Success}
import io.circe._
import io.circe.parser._
import io.circe.syntax._
//import org.rebeam.tree.ref.{Mirror, MirrorCodec}

object ServerRootComponent {

  trait RootSource[U, R, D <: Delta[U, R]] {
    def rootFor(rootModel: R, parent: Parent[U, R, D]): Root
  }

//  private class MirrorRootSource extends RootSource[Mirror] {
//    def rootFor(rootModel: Mirror, parent: Parent[Mirror]): Root = new Root {
//      def cursorAt[A, L](ref: TreeRef[A], location: L)
//                        (implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, L]] = {
//        rootModel.apply(ref).map { data =>
//          Cursor[A, L](MirrorParent[A](parent, ref), data, location, this)
//        }
//      }
//      def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid] = {
//        refGuids.flatMap(refGuid => rootModel.revisionOf(refGuid).map(refGuid -> _)).toMap
//      }
//    }
//  }
//
//  implicit val mirrorRootSource: RootSource[Mirror] = new MirrorRootSource
//
//  private class MirrorAndIdRootSource[M] extends RootSource[MirrorAndId[M]] {
//    // Keep the same lens instance - reduces changes in cursor data, reducing redraws
//    private val lens = MirrorAndId.mirror[M]
//    def rootFor(rootModel: MirrorAndId[M], parent: Parent[MirrorAndId[M]]): Root = new Root {
//      def cursorAt[A, L](ref: TreeRef[A], location: L)
//                        (implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, L]] = {
//        rootModel.mirror.apply(ref).map { data =>
//          val lensParent = LensNParent[MirrorAndId[M], Mirror](parent, lens)
//          Cursor[A, L](MirrorParent[A](lensParent, ref), data, location, this)
//        }
//      }
//      def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid] = {
//        refGuids.flatMap(refGuid => rootModel.mirror.revisionOf(refGuid).map(refGuid -> _)).toMap
//      }
//    }
//  }
//
//  implicit def mirrorAndIdRootSource[M]: RootSource[MirrorAndId[M]] = new MirrorAndIdRootSource

  private class NoRootSource[U, R, D <: Delta[U, R]] extends RootSource[U, R, D] {
    def rootFor(rootModel: R, parent: Parent[U, R, D]): Root = Cursor.RootNone
  }

  def noRootSource[U, R, D <: Delta[U, R]]: RootSource[U, R, D] = new NoRootSource[U, R, D]

  case class Props[U, R, D <: Delta[U, R], P](p: P, render: Cursor[U, R, D, P] => ReactElement, wsUrl: String, noData: ReactElement)

  case class State[U, R, D <: Delta[U, R]](clientState: Option[ClientState[U, R, D]], ws: Option[WebSocket], tick: Option[SetIntervalHandle])

  class Backend[U, R, D <: Delta[U, R], P](scope: BackendScope[Props[U, R, D, P], State[U, R, D]])
    (implicit
     decoder: Decoder[R],
     deltaDecoder: Decoder[D],
     deltaEncoder: Encoder[D],
     idGen: ModelIdGen[R],
     contextSource: DeltaIOContextSource,
     rootSource: RootSource[U, R, D],
     refAdder: RefAdder[U, R],
     searchable: Searchable[R, Guid]) {

    implicit val cme = clientMsgEncoder[U, R, D]

    val deltaToCallback: D => Callback =
      (delta: D) =>
        for {
          s <- scope.state
          _ <- s.clientState match {
            case None =>
              // TODO implement
              Callback{println("Delta before we have a clientState! Should queue deltas?")}

            case Some(cs) => {
              //SIDE-EFFECT: Note this is the point at which we generate the context
              val (newCS, id) = cs.apply(delta, contextSource.getContext)
              val dij = DeltaAndId[U, R, D](delta, id)
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

    val rootParent = RootParent[U, R, D](deltaToCallback)

    def render(props: Props[U, R, D, P], state: State[U, R, D]) = {
      state.clientState.map { cs =>
        val cursorP: Cursor[U, R, D, P] = Cursor(rootParent, cs.model, props.p, rootSource.rootFor(cs.model, rootParent))
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
            json => updateDecoder[U, R, D].decodeJson(json).fold(
              df => println("Could not decode JSON from server " + df + ":\n" + msg),
              update => {

                val newCSX = direct.state.clientState.fold(
                  ClientState.fromFirstUpdate[U, R, D](update)
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

  def factory[U, R, D <: Delta[U, R], P]
    (noData: ReactElement, wsUrl: String)
    (render: Cursor[U, R, D, P] => ReactElement)
    (implicit
     decoder: Decoder[R],
     deltaDecoder: Decoder[D],
     deltaEncoder: Encoder[D],
     idGen: ModelIdGen[R],
     contextSource: DeltaIOContextSource,
     rootSource: RootSource[U, R, D],
     refAdder: RefAdder[U, R],
     searchable: Searchable[R, Guid]) = {
    val c = ctor[U, R, D, P](decoder, deltaDecoder, deltaEncoder, idGen, contextSource, rootSource, refAdder, searchable)
    (page: P) => c(Props[U, R, D, P](page, render, wsUrl, noData))
  }


  def apply[U, R, D <: Delta[U, R]]
  (noData: ReactElement, wsUrl: String)
  (render: Cursor[U, R, D, Unit] => ReactElement)
  (implicit
   decoder: Decoder[R],
   deltaDecoder: Decoder[D],
   deltaEncoder: Encoder[D],
   idGen: ModelIdGen[R],
   contextSource: DeltaIOContextSource,
   rootSource: RootSource[U, R, D],
   refAdder: RefAdder[U, R],
   searchable: Searchable[R, Guid]) = {
    val c = ctor[U, R, D, Unit](decoder, deltaDecoder, deltaEncoder, idGen, contextSource, rootSource, refAdder, searchable)
    c(Props[U, R, D, Unit]((), render, wsUrl, noData))
  }

  //Just make the component constructor - props to be supplied later to make a component
  def ctor[U, R, D <: Delta[U, R], P]
    (implicit
     decoder: Decoder[R],
     deltaDecoder: Decoder[D],
     deltaEncoder: Encoder[D],
     idGen: ModelIdGen[R],
     contextSource: DeltaIOContextSource,
     rootSource: RootSource[U, R, D],
     refAdder: RefAdder[U, R],
     searchable: Searchable[R, Guid]) = ReactComponentB[Props[U, R, D, P]]("ServerRootComponent")
    .initialState(State[U, R, D](None, None, None))
    .backend(new Backend[U, R, D, P](_)(decoder, deltaDecoder, deltaEncoder, idGen, contextSource, rootSource, refAdder, searchable))
    .render(s => s.backend.render(s.props, s.state))
    .componentDidMount(_.backend.start)
    .componentWillUnmount(_.backend.end)
    .build

}
