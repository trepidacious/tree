package org.rebeam.tree.server.demo

import org.http4s._
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.websocket._
import org.http4s.websocket.WebsocketBits._

import org.http4s.server.staticcontent
import org.http4s.server.staticcontent.ResourceService.Config

import scala.concurrent.duration._
import scalaz.concurrent.{Strategy, Task}
import scalaz.stream.async.unboundedQueue
import scalaz.stream.time.awakeEvery
import scalaz.stream.{DefaultScheduler, Exchange, Process, Sink}

//import scalaz._
//import Scalaz._

object ServerApp extends App {

  val apiService = HttpService {

    case GET -> Root / "hello" =>
      Ok("Hello world.")

    case req@ GET -> Root / "ws" =>
      val src = awakeEvery(1.seconds)(Strategy.DefaultStrategy, DefaultScheduler).map{ d => Text(s"Ping! $d") }
      val sink: Sink[Task, WebSocketFrame] = Process.constant {
        case Text(t, _) => Task.delay(println(t))
        case f          => Task.delay(println(s"Unknown type: $f"))
      }
      WS(Exchange(src, sink))

    case req@ GET -> Root / "wsecho" =>
      val q = unboundedQueue[WebSocketFrame]
      val src = q.dequeue.collect {
        case Text(msg, _) => Text("Echoing: " + msg)
      }
      WS(Exchange(src, q.enqueue))

  }

  private def cachedResource(config: Config): HttpService = {
    val cachedConfig = config.copy(cacheStartegy = staticcontent.MemoryCache())
    staticcontent.resourceService(cachedConfig)
  }

  val resources = cachedResource(Config("/", "/"))
  // val polymerDist = staticcontent.resourceService(Config("/polymer/dist", "/"))

  val resourcesService: HttpService = HttpService {
    case r @ GET -> _ if r.pathInfo.isEmpty => resourcesService(r.withPathInfo("index.html"))
    case r @ GET -> _ if r.pathInfo.endsWith("/") => resourcesService(r.withPathInfo(r.pathInfo + "index.html"))
    case r @ GET -> _ => resources(r)
  }

  // val apiCORS = CORS(apiService)
  // val polymerCORS = CORS(polymerService)

  BlazeBuilder.bindHttp(8080, "0.0.0.0")
    .withWebSockets(true)
    .mountService(apiService, "/api")
    .mountService(resourcesService, "/")
    // .mountService(apiCORS, "/api")
    // .mountService(polymerCORS, "/")
    .run
    .awaitShutdown()

}
