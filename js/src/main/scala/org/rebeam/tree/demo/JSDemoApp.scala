package org.rebeam.tree.demo

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.rebeam.tree.view._
import org.scalajs.dom
import upickle.default._

import scala.language.higherKinds
import scala.scalajs.js.JSApp

import org.rebeam.tree.view.View._

object JSDemoApp extends JSApp {

  def main(): Unit = {

    val StreetView = cursorView[Street]("StreetView") { c =>
      <.div(
        <.div("Street: " + c.model.number + ", " + c.model.name),
        IntView(c.zoom("number", Street.number).label("Number")),
        TextView(c.zoom("name", Street.name).label("Name")),
        ActButton("Number multiple", c.act(StreetAction.NumberMultiple(10))),
        ActButton("Capitalise", c.act(StreetAction.Capitalise))
      )
    }

    val AddressView = TreeRootComponent(Address(Street("OLD STREET", 1)), "api/address"){
      addressCursor => {
        val streetCursor = addressCursor.zoom("street", Address.street)
        <.div(
          <.h1("Address"),
          StreetView(streetCursor)
        )
      }
    }

    val mountNode = dom.document.getElementsByClassName("content")(0)

    ReactDOM.render(AddressView, mountNode)
  }

}
