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
import org.rebeam.tree.demo.StreetAction.NumberMultiple
import upickle.Js

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

  def view[A](name: String, overlay: Boolean = false)(render: A => ReactElement) =
    ReactComponentB[A](name).render_P(render).build

  implicit def cursorReuse[A]: Reusability[Cursor[A]] = Reusability.by_==

  def cursorView[A](name: String, overlay: Boolean = false)(render: Cursor[A] => ReactElement) = if (overlay) {
    ReactComponentB[Cursor[A]](name).render_P(render).configure(Reusability.shouldComponentUpdateWithOverlay).build
  } else {
    ReactComponentB[Cursor[A]](name).render_P(render).build
  }

  def main(): Unit = {

    val PlainStringView = cursorView[String]("StringView") { c =>
      <.input(
        ^.`type` := "text",
        ^.value := c.model,
        ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value))
      )
    }

    val LabelledStringView = view[(String, Cursor[String])]("LabelledStringView") { p =>
      <.form(
        <.div(
          ^.classSet1("mdl-textfield mdl-js-textfield mdl-textfield--floating-label"),
          <.input(
            ^.id := "string-view-input",
            ^.classSet1("mdl-textfield__input"),
            ^.`type` := "text",
            ^.value := p._2.model,
            ^.onChange ==> ((e: ReactEventI) => p._2.set(e.target.value))
          ),
          <.label(
            ^.classSet1("mdl-textfield__label"),
            ^.`for` := "string-view-input",
            p._1
          )
        )
      )
    }

    val StringView = cursorView[String]("StringView") { c =>
      <.form(
        <.div(
          ^.classSet1("mdl-textfield mdl-js-textfield mdl-textfield--floating-label"),
          <.input(
            ^.id := "string-view-input",
            ^.classSet1("mdl-textfield__input"),
            ^.`type` := "text",
            ^.value := c.model,
            ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value))
          ),
          <.label(
            ^.classSet1("mdl-textfield__label"),
            ^.`for` := "string-view-input",
            "Label"
          )
        )
      )
    }

    val IntView = cursorView[Int]("IntView"){ c =>
      <.input(
        ^.`type` := "number",
        ^.value := c.model.toString,
        ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value.toInt))
      )
    }

    def ActButton(s: String, c: Callback) =
      <.button(
        s,
        ^.onClick ==> ((e: ReactEventI) => e.preventDefaultCB >> c),
        ^.classSet1("mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect mdl-button--accent")
      )

    val StreetView = cursorView[Street]("StreetView") { c =>
      <.div(
        <.div("Street: " + c.model.number + ", " + c.model.name),
        IntView(c.zoom("number", Street.number)),
        LabelledStringView(("Name", c.zoom("name", Street.name))),
        ActButton("Number multiple", c.act(StreetAction.NumberMultiple(10))),
        ActButton("Capitalise", c.act(StreetAction.Capitalise))
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

    val mountNode = dom.document.getElementsByClassName("content")(0)
    
    ReactDOM.render(AddressView(), mountNode)
  }

}
