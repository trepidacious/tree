package org.rebeam.tree.demo

import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.rebeam.tree.view.Nav._

import com.payalabs.scalajs.react.mdl.MaterialAble

object DemoRoutes {

  sealed trait Page
  case object Home          extends Page
  case object Address       extends Page

  val title = "Tree"

  val routerConfig = RouterConfigDsl[Page].buildConfig { dsl =>
    import dsl._

    (trimSlashes
      | staticRoute(root,   Home) ~> render(DemoViews.HomeView())
      | staticRoute("#address", Address) ~> render(DemoViews.AddressView)
      )

      .notFound(redirectToPage(Home)(Redirect.Replace))
      .renderWith(layout)
      .verify(Home, Address)
  }

  val navs = Map(
    "Home" -> Home,
    "Address" -> Address
  )

  val navPageMenu = navMenu[Page]
  val navPageDrawer = navDrawer[Page]

  def layout(ctl: RouterCtl[Page], r: Resolution[Page]) = {
    val np = NavProps(ctl, r.page, navs, title)

    navLayout(
      navPageMenu(np),
      navPageDrawer(np),
      navContents(r.render())
    ).material
  }

  val baseUrl = BaseUrl.fromWindowOrigin_/

  def router = Router(baseUrl, routerConfig.logToConsole)

}
