package org.rebeam.tree.demo

import java.io.File

import org.http4s._
import org.http4s.dsl._
import org.http4s.server.ServerApp
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.staticcontent._
import org.http4s.server.websocket._
import org.http4s.websocket.WebsocketBits._
import org.rebeam.tree.Moment
import org.rebeam.tree.server.{TreeStore, TreeStoreValueExchange}
import org.rebeam.tree.view.MaterialColor

import scala.concurrent.duration._
import scalaz.concurrent.{Strategy, Task}
import scalaz.stream.async.unboundedQueue
import scalaz.stream.time.awakeEvery
import scalaz.stream.{DefaultScheduler, Exchange, Process, Sink}

import DemoData._

object ServerDemoApp extends ServerApp {

  val address = new TreeStore(Address(Street("OLD STREET", 1)))

//  val todoList = {
//    val time = System.currentTimeMillis()
//    TodoList(
//      "Todo", "trepidacious@gmail.com", MaterialColor.Amber(),
//      (1 to 10).map(i => {
//        Todo(i, "Item " + i, Moment(time - 60000 * (10 - i)))
//      }).toList,
//      11
//    )
//  }
//
//  val todoListStore = new TreeStore(todoList)

  val apiService = HttpService {

    case GET -> Root / "hello" =>
      Ok("Hello world!")

    case GET -> Root / "pwd" =>
      Ok(System.getProperty("user.dir"))

//    case GET -> Root / "todolist" =>
//      WS(TreeStoreValueExchange(todoListStore))

    case GET -> Root / "address" =>
      WS(TreeStoreValueExchange(address))

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

  //This will serve from java resources, so work in a jar
  //We can also set cacheStartegy = staticcontent.MemoryCache() in the Config
  //val resources = resourceService(ResourceService.Config("", "/"))

  //This serves directly from development resources directory, so will update
  //when we change original resources files and refresh browser
  val resources = fileService(FileService.Config("src/main/resources", "/"))

  val resourcesService: HttpService = HttpService {
    case r @ GET -> _ if r.pathInfo.isEmpty => resourcesService(r.withPathInfo("index.html"))
    case r @ GET -> _ if r.pathInfo.endsWith("/") => resourcesService(r.withPathInfo(r.pathInfo + "index.html"))
    case r @ GET -> _ => resources(r)
  }

  //Serve our scala-js from js project target - kind of messy
  val scalajs = fileService(FileService.Config(new File(System.getProperty("user.dir")).getParent + "/js/target/scala-2.11", "/scalajs"))
  val scalajsService: HttpService = HttpService {
    case r @ GET -> _ => scalajs(r)
  }

  // val apiCORS = CORS(apiService)

  def server(args: List[String]) =
    BlazeBuilder.bindHttp(8080, "0.0.0.0")
      .withWebSockets(true)
      .mountService(apiService, "/api")
      .mountService(resourcesService, "/")
      .mountService(scalajsService, "/")
      // .mountService(apiCORS, "/api")
      .start

}
