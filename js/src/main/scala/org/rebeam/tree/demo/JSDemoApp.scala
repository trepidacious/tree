package org.rebeam.tree.demo

import japgolly.scalajs.react._
import org.scalajs.dom

import scala.scalajs.js.JSApp

object JSDemoApp extends JSApp {

  def main(): Unit = {
    val mountNode = dom.document.getElementsByClassName("content")(0)
    ReactDOM.render(DemoViews.AddressView, mountNode)
  }

}
