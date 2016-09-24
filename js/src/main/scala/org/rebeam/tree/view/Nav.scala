package org.rebeam.tree.view

import japgolly.scalajs.react.{ReactComponentB, ReactElement}
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.prefix_<^._

object Nav {

  case class NavProps[P](routerCtl: RouterCtl[P], page: P, navs: Map[String, P], title: String)

  //Reusable if all fields are equal except routerCtl, where we use its own reusability
  implicit def navPropsReuse[P]: Reusability[NavProps[P]] = Reusability.fn{
    case (a, b) if a eq b => true // First because most common case and fastest
    case (a, b) if a.page == b.page && a.navs == b.navs && a.title == b.title => RouterCtl.reusability[P].test(a.routerCtl, b.routerCtl)
    case _ => false
  }

  def navMenu[P] = ReactComponentB[NavProps[P]]("Menu")
    .render_P { p =>

      def nav(name: String, target: P) = {
        <.nav(
          ^.cls := "mdl-navigation mdl-layout--large-screen-only",
          //TODO get highlighting of selected link
          <.span(
            ^.cls := "mdl-navigation__link",
            (if (p.page == target) ">" else "") + name,
            p.routerCtl setOnClick target
          )
        )
      }

      <.header(
        ^.cls := "mdl-layout__header",
        <.div(
          ^.cls := "mdl-layout__header-row",

          // Title
          <.span(^.cls := "mdl-layout-title", p.title),

          // Add spacer, to align navigation to the right
          <.div(^.cls := "mdl-layout-spacer")
        )(
          // Navigation. We hide it in small screens.
          p.navs.map(n => nav(n._1, n._2))
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  def navDrawer[P] = ReactComponentB[NavProps[P]]("Drawer")
    .render_P { p =>

      def nav(name: String, target: P) = {
        <.span(
          ^.cls := "mdl-navigation__link",
          (if (p.page == target) ">" else "") + name,
          p.routerCtl setOnClick target
        )
      }

      <.div(
        ^.cls := "mdl-layout__drawer",
        <.span(
          ^.cls := "mdl-layout-title",
          p.title
        ),
        <.nav(^.cls := "mdl-navigation")(
          p.navs.map(n => nav(n._1, n._2))
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build

  val navLayout = <.div(^.cls := "mdl-layout mdl-js-layout mdl-layout--fixed-header")

  def navContents(r: ReactElement) =
    <.main(
      ^.cls := "mdl-layout__content",
      <.div(
        ^.cls := "page-content",
        r
      )
    )

}
