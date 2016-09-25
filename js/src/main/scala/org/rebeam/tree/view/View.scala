package org.rebeam.tree.view

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import com.payalabs.scalajs.react.mdl.MaterialAble

object View {
  def view[A](name: String, overlay: Boolean = false)(render: A => ReactElement) =
    ReactComponentB[A](name).render_P(render).build

//  implicit def cursorReuse[A]: Reusability[Cursor[A]] = Reusability.by_==
//  def cursorView[A](name: String, overlay: Boolean = false)(render: Cursor[A] => ReactElement) = if (overlay) {
//    ReactComponentB[Cursor[A]](name).render_P(render).configure(Reusability.shouldComponentUpdateWithOverlay).build
//  } else {
//    ReactComponentB[Cursor[A]](name).render_P(render).build
//  }

  def cursorView[A](name: String, overlay: Boolean = false)(render: Cursor[A] => ReactElement) =
    ReactComponentB[Cursor[A]](name).render_P(render).build

  def labelledCursorView[A](name: String)(render: LabelledCursor[A] => ReactElement) =
    ReactComponentB[LabelledCursor[A]](name).render_P(render).build

  def staticView(e: ReactElement, name: String = "StaticView") = ReactComponentB[Unit](name)
    .render(_ => e)
    .build

  def dynamicView(name: String)(render: => ReactElement) =
    ReactComponentB[Unit](name).render_P(_ => render).build

  //  val PlainStringView = cursorView[String]("StringView") { c =>
//    <.input(
//      ^.`type` := "text",
//      ^.value := c.model,
//      ^.onChange ==> ((e: ReactEventI) => c.set(e.target.value))
//    )
//  }

  val TextView = view[LabelledCursor[String]]("TextView") { p =>
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

  val IntView = view[LabelledCursor[Int]]("IntView") { p =>
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
