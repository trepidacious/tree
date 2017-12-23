package org.rebeam.tree.util

import scala.collection.mutable.ListBuffer

object TreeUtils {

  implicit class EnhancedList[A](l: List[A]) {

    def swapped(oldIndex: Int, newIndex: Int): List[A] = {
      val lb = ListBuffer(l: _*)
      val e = lb.remove(oldIndex)
      lb.insert(newIndex, e)
      lb.toList
    }

    def inserted(i: Int, a: A): List[A] = {
      if (i <= 0) {
        a :: l
      } else if (i >= l.size) {
        l ++ List(a)
      } else {
        val (front, back) = l.splitAt(i)
        front ++ List(a) ++ back
      }
    }

    def deleted(i: Int): List[A] = {
      if (i < 0 || i >= l.size) {
        l
      } else {
        val (front, back) = l.splitAt(i)
        front ++ back.tail
      }
    }

  }

}
