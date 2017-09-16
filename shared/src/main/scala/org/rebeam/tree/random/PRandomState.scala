package org.rebeam.tree.random

import cats.data.State

object PRandomState {

  type PRandomState[A] = State[PRandom, A]

  val int: PRandomState[Int] = State(_.int)
  def intUntil(bound: Int): PRandomState[Int] = State(_.intUntil(bound))
  val long: PRandomState[Long] = State(_.long)
  val boolean: PRandomState[Boolean] = State(_.boolean)
  val float: PRandomState[Float] = State(_.float)
  val double: PRandomState[Double] = State(_.double)

}
