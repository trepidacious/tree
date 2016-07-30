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

  def main(): Unit = {

    implicit def cursorReuse[A]: Reusability[Cursor[A]] = Reusability.by_==

    val StringView =
      ReactComponentB[Cursor[String]]("StringView")
        .render_P(c =>
          <.input(
            ^.`type` := "text",
            ^.value := c.model,
            ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value))
          )
        )
        .configure(Reusability.shouldComponentUpdateWithOverlay)
        .build

    val IntView =
      ReactComponentB[Cursor[Int]]("IntView")
        .render_P(c =>
          <.input(
            ^.`type` := "number",
            ^.value := c.model.toString,
            ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value.toInt))
          )
        )
        .configure(Reusability.shouldComponentUpdateWithOverlay)
        .build

    val StreetView =
      ReactComponentB[Cursor[Street]]("StreetView")
        .render_P(c =>
          {
            <.div(
              <.div("Street: " + c.model.number + ", " + c.model.name),
              IntView(c.zoom("number", Street.number)),
              StringView(c.zoom("name", Street.name))
            )
          }
        )
        .build

    class AddressBackend(scope: BackendScope[Unit, Address]) {

      val deltaToCallback = (delta: Delta[Address], deltaJs: Js.Value) =>
        scope.modState(delta.apply) >> Callback(println("Delta >> " + deltaJs.toString))

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
      .renderBackend[AddressBackend]  // ← Use Backend class and backend.render
      .build

    // class StreetBackend($: BackendScope[Street, Unit]) {
    //   def render(s: Street) =
    //   <.div("Street: " + s.number + ", " + s.name)
    // }
    // 
    // val StreetView = ReactComponentB[Unit]("Example")
    //   .initialState(a)
    //   .renderBackend[AddressBackend]  // ← Use Backend class and backend.render
    //   .build


    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render($ => <.div("Hello ", $.props))
      .build

    val mountNode = dom.document.getElementsByClassName("demo")(0)
    
    ReactDOM.render(AddressView(), mountNode)
  }

//   def appendPar(targetNode: dom.Node, text: String): Unit = {
//     targetNode.appendChild(p(text).render)
//   }
// 
//   def main(): Unit = {
//     
//     val a = Address(Street("OLD STREET", 1))
// 
//     //Build a delta using cursor, just adding resulting new models to paragraphs
//     //alongside the encoded JSON, then applying the delta read from encoded js
//     //and comparing the result, to show that server-side replication of the
//     //delta would work
//     val callback = (delta: Delta[Address], deltaJs: Js.Value) => {
//       val aDelta = delta.apply(a)
//       appendPar(document.body, "After cursor delta applied directly: " + aDelta)
//       appendPar(document.body, "Cursor delta encoded as: " + deltaJs)
//       
//       val aDeltaJs = Address.addressDeltaReader.readDelta(deltaJs).apply(a)
//       appendPar(document.body, "After cursor delta applied using JSON: " + aDeltaJs)
//       
//       appendPar(document.body, "Equal? " + (aDelta == aDeltaJs))
//       
//     }
//     val root = RootParent(callback)
//     val addressCursor = Cursor(root, a)
//     val streetCursor = addressCursor.zoom("street", Address.street)
//     
//     streetCursor.set(Street("New street set using cursor", 9002))
//     streetCursor.set(Street("Another new street set using cursor", 9003))
//     streetCursor.act(StreetActionNumberMultiple(3))
//   }
}
