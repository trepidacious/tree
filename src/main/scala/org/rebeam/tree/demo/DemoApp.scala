package org.rebeam.tree.demo

import org.scalajs.dom

import scala.scalajs.js.JSApp
import org.rebeam.tree._
import monocle.macros.Lenses
import upickle.default._

import scala.language.higherKinds
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import upickle.Js

@Lenses case class Street(name: String, number: Int)
@Lenses case class Address(street: Street)
@Lenses case class Company(address: Address)
@Lenses case class Employee(name: String, company: Company)

sealed trait StreetAction extends Delta[Street]

case class StreetActionNumberMultiple(multiple: Int) extends StreetAction {
  def apply(s: Street): Street = Street(s.name, s.name.length * multiple)
}

object Street {
  import DeltaReaders._
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

object DemoApp extends JSApp {

  implicit def cursorReuse[A]: Reusability[Cursor[A]] = Reusability.by_==

  def view[A](name: String, overlay: Boolean = true)(render: Cursor[A] => ReactElement) = if (overlay) {
    ReactComponentB[Cursor[A]](name).render_P(render).configure(Reusability.shouldComponentUpdateWithOverlay).build
  } else {
    ReactComponentB[Cursor[A]](name).render_P(render).build
  }

  def main(): Unit = {

    val StringView = view[String]("StringView") { c =>
      <.input(
        ^.`type` := "text",
        ^.value := c.model,
        ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value))
      )
    }

    val IntView = view[Int]("IntView"){ c =>
      <.input(
        ^.`type` := "number",
        ^.value := c.model.toString,
        ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value.toInt))
      )
    }

    val StreetView = view[Street]("StreetView") { c =>
      <.div(
        <.div("Street: " + c.model.number + ", " + c.model.name),
        IntView(c.zoom("number", Street.number)),
        StringView(c.zoom("name", Street.name))
      )
    }

    class AddressBackend(scope: BackendScope[Unit, Address]) {

      //Apply the delta, and print its Json. In a real implementation this
      //would still apply the delta, but would also send the Json to a server
      //to attempt to "commit" the change. The state might actually store a
      //tentative Address as modified locally, and a last-known authoritative
      //Address from the server, to allow reverting local modifications if they
      //are not confirmed, or merging them if the server reports it merged them.
      val deltaToCallback = (delta: Delta[Address], deltaJs: Js.Value) =>
        scope.modState(delta.apply) >> Callback(println("Delta >> " + deltaJs.toString))

      //The parent for this root component - we sit at the root of the data model
      //(Address), and are responsible for holding it as state and
      val rootParent = RootParent(deltaToCallback)

      def render(a: Address) = {
        val addressCursor = Cursor(rootParent, a)
        val streetCursor = addressCursor.zoom("street", Address.street)
        <.div(
          <.h1("Address"),
          StreetView(streetCursor)
        )
      }
    }

    val AddressView = ReactComponentB[Unit]("AddressView")
      .initialState(Address(Street("OLD STREET", 1)))
      .renderBackend[AddressBackend]  // ← Use AddressBackend class and backend.render
      .build

    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render($ => <.div("Hello ", $.props))
      .build

    val mountNode = dom.document.getElementsByClassName("demo")(0)
    
    ReactDOM.render(AddressView(), mountNode)
  }

}
