package org.rebeam.tree

object TreeUtils {

  implicit class EnrichedString(val s: String) extends AnyVal {
    def toIntOpt: Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: NumberFormatException => None
    }
    def toDoubleOpt: Option[Double] = try {
      Some(s.toDouble)
    } catch {
      case _: NumberFormatException => None
    }
  }
}
