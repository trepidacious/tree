package org.rebeam.tree.view

import japgolly.scalajs.react.Callback
import scala.language.implicitConversions

/**
  * Action is a trait with just `def callback: Callback`.
  * This should be used where possible instead of `Callback` in props of components. Two callback instances are never equal, even if they do the same thing. However two Action instances should be equal if and only if they will produce Callbacks that achieve the same effect.
  * This can be achieved by implementing `Action` with case classes that contain immutable data that is equal when the callback would have the same effect, and produce the callback from just that data on demand (e.g. as a lazy val).
  * An implicit conversion is provided from `Action` to `Callback`.
  */
trait Action {
  def callback: Callback
}

object Action {
  implicit def toCallback(a: Action): Callback = a.callback
}
