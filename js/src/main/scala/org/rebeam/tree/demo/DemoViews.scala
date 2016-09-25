package org.rebeam.tree.demo

import japgolly.scalajs.react.vdom.prefix_<^._
import org.rebeam.tree.view.View._
import org.rebeam.tree.view.WSRootComponent
import DemoData._

object DemoViews {

  val StreetView = cursorView[Street]("StreetView") { c =>
    <.div(
      IntView(c.zoom("number", Street.number).label("Number")),
      TextView(c.zoom("name", Street.name).label("Name")),
      <.p(ActButton("Number multiple", c.act(StreetAction.NumberMultiple(10): StreetAction))),
      <.p(ActButton("Capitalise", c.act(StreetAction.Capitalise: StreetAction)))
    )
  }

  val noData = <.div(<.p("Retrieving data from server..."))
  val AddressView = WSRootComponent[Address](noData, "api/address") {
    addressCursor => {
      val streetCursor = addressCursor.zoom("street", Address.street)
      <.div(
        <.h3("Address"),
        StreetView(streetCursor)
      )
    }
  }

  val HomeView = staticView(
    <.div(
      <.h3("Home")
    )
  )

}
