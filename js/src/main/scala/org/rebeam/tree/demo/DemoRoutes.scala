package org.rebeam.tree.demo

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._

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

  def layout(ctl: RouterCtl[Page], r: Resolution[Page]) = {
    <.div(
      ^.cls := "mdl-layout mdl-js-layout mdl-layout--fixed-header",
      navMenu(ctl),
      navDrawer(ctl),
      <.main(
        ^.cls := "mdl-layout__content",
        <.div(
          ^.cls := "page-content",
          r.render()
        )
      )
    )
  }

  val navMenu = ReactComponentB[RouterCtl[Page]]("Menu")
    .render_P { ctl =>

      def nav(name: String, target: Page) = {
        <.nav(
          ^.cls := "mdl-navigation mdl-layout--large-screen-only",
          //TODO get highlighting of selected link
          <.span(
            ^.cls := "mdl-navigation__link is-active",
            name,
            ctl setOnClick target
          )
        )
      }

      <.header(
        ^.cls := "mdl-layout__header",
        <.div(
          ^.cls := "mdl-layout__header-row",

          // Title
          <.span(^.cls := "mdl-layout-title", title),

          // Add spacer, to align navigation to the right
          <.div(^.cls := "mdl-layout-spacer"),

          // Navigation. We hide it in small screens.
          nav("Home",               Home),
          nav("Address",            Address)
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  val navDrawer = ReactComponentB[RouterCtl[Page]]("Drawer")
    .render_P { ctl =>

      def nav(name: String, target: Page) = {
        <.span(
          ^.cls := "mdl-navigation__link",
          name,
          ctl setOnClick target
        )
      }

      <.div(
        ^.cls := "mdl-layout__drawer",
        <.span(
          ^.cls := "mdl-layout-title",
          title
        ),
        <.nav(
          ^.cls := "mdl-navigation",
          nav("Home",               Home),
          nav("Address",            Address)
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  val baseUrl = BaseUrl.fromWindowOrigin_/

  def router = Router(baseUrl, routerConfig.logToConsole)

}
