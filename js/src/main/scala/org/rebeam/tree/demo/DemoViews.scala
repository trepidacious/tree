package org.rebeam.tree.demo

import japgolly.scalajs.react.vdom.prefix_<^._
import org.rebeam.tree.view.View._
import org.rebeam.tree.view.WSRootComponent
import DemoData._
import org.rebeam.tree.demo.DemoData.Priority.{High, Low, Medium}

import com.payalabs.scalajs.react.mdl.MaterialAble

import org.rebeam.tree.DeltaCodecs._

object DemoViews {

  val StreetView = cursorView[Street]("StreetView") { c =>
    <.div(
      <.p("Blah"),
      intView(c.zoomN(Street.numberN).label("Number")),
      textView(c.zoomN(Street.nameN).label("Name")),
      <.p(actButton("Number multiple", c.act(StreetAction.NumberMultiple(10): StreetAction))),
      <.p(actButton("Capitalise", c.act(StreetAction.Capitalise: StreetAction)))
    )
  }

  val noAddress = <.div(
    <.h3("Address"),
    spinner()
  )
  val AddressView = WSRootComponent[Address](noAddress, "api/address") {
    addressCursor => {
      val streetCursor = addressCursor.zoomN(Address.streetN)
      <.div(
        <.h3("Address"),
        StreetView(streetCursor)
      )
    }
  }

  val HomeView = staticView (
    <.div (
      <.h3("Home")
    )
  )

  val TodoView = cursorView[Todo]("TodoView") { c =>
    def tdText(xs: TagMod*) = <.td(^.cls := "mdl-data-table__cell--non-numeric")(xs)

    def tdPriority(p: Priority) =
      <.td(
        ^.classSet1(
          "mdl-data-table__cell--priority",
          "mdl-data-table__cell--priority-high" -> (p == High)
        ),
        <.i(
          ^.cls := "material-icons",
          p match {
            case High => "error"
            case Medium => ""
            case Low => "pause"
          }
        )
      )

    val t = c.model
    <.tr(
      tdText(t.completed.fold(" ")(_ => "x")),
      tdText("#" + t.id),
      tdText(textViewPlainLabel(c.zoomN(Todo.nameN).label("Name"))),
      tdPriority(t.priority)
    )
  }

  val TodoListTableView = cursorView[TodoList]("TodoListTableView") { c => {
    val itemsCursor = c.zoomN(TodoList.itemsN)
    <.table(
      ^.cls := "mdl-data-table mdl-js-data-table",  //mdl-data-table--selectable mdl-shadow--2dp
      <.thead(
        <.tr(
          <.th(^.cls := "mdl-data-table__cell--non-numeric", "Done?"),
          <.th(^.cls := "mdl-data-table__cell--non-numeric", "Id"),
          <.th(^.cls := "mdl-data-table__cell--non-numeric", "Name"),
          <.th(^.cls := "mdl-data-table__cell--non-numeric", "Priority")
        )
      ),
      <.tbody(
        c.model.items.zipWithIndex.flatMap {
          case(todo, i) =>
            itemsCursor.zoomI[Todo](i).map(
              TodoView.withKey(todo.id)(_)
            )
        }
      )
    ).material
  }}

  val noTodoList = <.div(
    <.h3("Todo"),
    spinner()
  )
  val TodoListView = WSRootComponent[TodoList](noTodoList, "api/todolist") {
    c => {
      <.div(
        <.h3("Todo List"),
        textView(c.zoomN(TodoList.nameN).label("Name")),
        textView(c.zoomN(TodoList.emailN).label("Email")),
        TodoListTableView(c)
      )
    }
  }


//  val TodoView = cursorView[Todo]("TodoView") { c =>
//    def td(s: String) = <.td(^.cls := "mdl-data-table__cell--non-numeric", s)
//
//    val t = c.model
//    <.tr(
//      td(t.completed.fold(" ")(_ => "x")),
//      td("#" + t.id),
//      td(t.name),
//      td(t.priority match {
//        case High => "!"
//        case Medium => ""
//        case Low => "?"
//      })
//    )
//  }

}
