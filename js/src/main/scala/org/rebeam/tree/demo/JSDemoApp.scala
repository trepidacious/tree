package org.rebeam.tree.demo

import org.scalajs.dom

import scala.scalajs.js.JSApp
import japgolly.scalajs.react._

object JSDemoApp extends JSApp {

  def main(): Unit = {
    val mountNode = dom.document.getElementsByClassName("content")(0)

    val router = DemoRoutes.router
    router() render mountNode

//    MaterialSwatch.AllFamiliesView() render mountNode

//    ReactDOM.render(DemoViews.todoListView, mountNode)
  }

}
