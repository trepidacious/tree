package org.rebeam.tree.demo

import japgolly.scalajs.react._
import org.scalajs.dom

import scala.scalajs.js.JSApp

import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

object JSDemoApp extends JSApp {

  sealed trait Page
  case object Home          extends Page
  case object Address       extends Page

  val routerConfig = RouterConfigDsl[Page].buildConfig { dsl =>
    import dsl._

//    def exampleRoutes: Rule =
//      (ExamplesJs.routes | ExamplesScala.routes)
//        .prefixPath_/("#examples").pmap[Page](Eg) { case Eg(e) => e }

    (trimSlashes
      | staticRoute(root,   Home) ~> render(DemoViews.HomeView())
      | staticRoute("#address", Address) ~> render(DemoViews.AddressView)
      )

      .notFound(redirectToPage(Home)(Redirect.Replace))
      .renderWith(layout)
      .verify(Home, Address)
  }

  def layout(ctl: RouterCtl[Page], r: Resolution[Page]) =
    <.div(
      navMenu(ctl),
      <.div(^.cls := "container", r.render()))

  val navMenu = ReactComponentB[RouterCtl[Page]]("Menu")
    .render_P { ctl =>

      def nav(name: String, target: Page) =
        <.li(
          ^.cls := "navbar-brand active",
          ctl setOnClick target,
          name)

      <.div(^.cls := "navbar navbar-default",
        <.ul(
          ^.cls := "navbar-header",
          nav("Home",               Home),
          nav("Address",            Address)
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  val baseUrl = BaseUrl.fromWindowOrigin_/

  def main(): Unit = {
    val mountNode = dom.document.body //dom.document.getElementsByClassName("content")(0)

    val router = Router(baseUrl, routerConfig.logToConsole)
    router() render mountNode


    //    ReactDOM.render(DemoViews.AddressView, mountNode)
  }

}
