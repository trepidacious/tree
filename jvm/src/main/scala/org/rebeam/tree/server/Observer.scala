package org.rebeam.tree.server

/**
  * Created by trepidacious on 10/11/2016.
  */
//Observer performs a side effect whenever a data item of type T is produced
trait Observer[T] {
  def observe(t: T): Unit
}
