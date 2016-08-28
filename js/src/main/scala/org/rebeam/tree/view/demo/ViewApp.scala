package org.rebeam.tree.view.demo

import org.scalajs.dom

import scala.scalajs.js.JSApp
import org.rebeam.tree._
import monocle.macros.Lenses
import upickle.default._

import scala.language.higherKinds
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import org.rebeam.tree.view._

@Lenses case class Street(name: String, number: Int)
@Lenses case class Address(street: Street)
@Lenses case class Company(address: Address)
@Lenses case class Employee(name: String, company: Company)

sealed trait StreetAction extends Delta[Street]

object StreetAction {
  case class NumberMultiple(multiple: Int) extends StreetAction {
    def apply(s: Street): Street = s.copy(number = s.name.length * multiple)
  }

  case object Capitalise extends StreetAction {
    def apply(s: Street): Street = s.copy(name = s.name.toLowerCase.capitalize)
  }
}

object Street {
  implicit val streetDeltaReader =
    DeltaReader.build[Street]
      .lens("name", Street.name)
      .lens("number", Street.number)
      .action[StreetAction]
}

object Address {
  import Street._

  implicit val addressDeltaReader =
    DeltaReader.build[Address]
      .lens("street", Address.street)
}

object Company {
}

object Employee {
}

object ViewApp extends JSApp {

  import org.rebeam.tree.view.View._

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

    val AddressView = TreeRootComponent(Address(Street("OLD STREET", 1)), "api/wsecho"){
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
