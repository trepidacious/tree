package org.rebeam.tree

object TreeUtils {

  implicit class EnrichedString(val s: String) extends AnyVal {
    def toIntOpt(str: String): Option[Int] = try {
      Some(str.toInt)
    } catch {
      case NumberFormatException => None
    }
    def toDoubleOpt(str: String): Option[Double] = try {
      Some(str.toDouble)
    } catch {
      case NumberFormatException => None
    }
  }

}
