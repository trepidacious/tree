package org.rebeam.tree.view

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import com.payalabs.scalajs.react.mdl.MaterialAble
import japgolly.scalajs.react.extra.Reusability

object View {
  def view[A](name: String, overlay: Boolean = true)(render: A => ReactElement) =
    ReactComponentB[A](name).render_P(render).build

  //We are careful to ensure that cursors remain equal if they are reusable
  implicit def cursorReuse[A]: Reusability[Cursor[A]] = Reusability.by_==
  implicit def labelledCursorReuse[A]: Reusability[LabelledCursor[A]] = Reusability.by_==

  def cursorView[A](name: String)(render: Cursor[A] => ReactElement) =
    ReactComponentB[Cursor[A]](name).render_P(render).configure(Reusability.shouldComponentUpdate).build

  def labelledCursorView[A](name: String, overlay: Boolean = false)(render: LabelledCursor[A] => ReactElement) =
    ReactComponentB[LabelledCursor[A]](name).render_P(render).configure(Reusability.shouldComponentUpdate).build

  def staticView(e: ReactElement, name: String = "StaticView") = ReactComponentB[Unit](name)
    .render(_ => e)
    .build

  def dynamicView(name: String)(render: => ReactElement) =
    ReactComponentB[Unit](name).render_P(_ => render).build

  val spinner = staticView(<.div(^.cls := "mdl-spinner mdl-js-spinner is-active").material, "Spinner")

  val TextView = labelledCursorView[String]("TextView") { p =>
    <.form(
      <.div(
        ^.classSet1("mdl-textfield mdl-js-textfield mdl-textfield--floating-label"),
        <.input(
          ^.id := "string-view-input",
          ^.classSet1("mdl-textfield__input"),
          ^.`type` := "text",
          ^.value := p.cursor.model,
          ^.onChange ==> ((e: ReactEventI) => p.cursor.set(e.target.value))
        ),
        <.label(
          ^.classSet1("mdl-textfield__label"),
          ^.`for` := "string-view-input",
          p.label
        )
      ).material
    )
  }

  val TextViewPlainLabel = labelledCursorView[String]("TextView") { p =>
    <.form(
      <.div(
        ^.classSet1("mdl-textfield mdl-js-textfield"),
        <.input(
          ^.id := "string-view-input",
          ^.classSet1("mdl-textfield__input"),
          ^.`type` := "text",
          ^.value := p.cursor.model,
          ^.onChange ==> ((e: ReactEventI) => p.cursor.set(e.target.value))
        ),
        <.label(
          ^.classSet1("mdl-textfield__label"),
          ^.`for` := "string-view-input",
          p.label
        )
      ).material
    )
  }

  val IntView = labelledCursorView[Int]("IntView") { p =>
    <.form(
      <.div(
        ^.classSet1("mdl-textfield mdl-js-textfield mdl-textfield--floating-label"),
        <.input(
          ^.id := "string-view-input",
          ^.classSet1("mdl-textfield__input"),
          ^.`type` := "number",
          ^.value := p.cursor.model.toString,
          ^.onChange ==> ((e: ReactEventI) => p.cursor.set(e.target.value.toInt))
        ),
        <.label(
          ^.classSet1("mdl-textfield__label"),
          ^.`for` := "string-view-input",
          p.label
        )
      ).material
    )
  }

//  val IntView = cursorView[Int]("IntView"){ c =>
//    <.input(
//      ^.`type` := "number",
//      ^.value := c.model.toString,
//      ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value.toInt))
//    )
//  }

  def ActButton(s: String, c: Callback, accent: Boolean = false, colored: Boolean = true) =
    <.button(
      s,
      ^.onClick ==> ((e: ReactEventI) => e.preventDefaultCB >> c),
      ^.classSet1(
        "mdl-button mdl-js-button mdl-button--raised mdl-js-ripple-effect",
        "mdl-button--accent" -> accent,
        "mdl-button--colored" -> colored
      )
    ).material

}
