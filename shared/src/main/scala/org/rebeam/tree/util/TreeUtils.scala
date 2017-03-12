package org.rebeam.tree.util

import scala.collection.mutable.ListBuffer

object TreeUtils {

  implicit class SwappableList[A](l: List[A]) {
    def swapped(oldIndex: Int, newIndex: Int): List[A] = {
      val lb = ListBuffer(l: _*)
      val e = lb.remove(oldIndex)
      lb.insert(newIndex, e)
      lb.toList
    }
  }

}
