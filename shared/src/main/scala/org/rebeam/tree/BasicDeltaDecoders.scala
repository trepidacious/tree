package org.rebeam.tree

import scala.language.higherKinds

object BasicDeltaDecoders {
  import DeltaCodecs._
  implicit val stringDeltaReader = value[String]
  implicit val booleanDeltaReader = value[Boolean]
  implicit val byteDeltaReader = value[Byte]
  implicit val shortDeltaReader = value[Short]
  implicit val intDeltaReader = value[Int]
  implicit val longDeltaReader = value[Long]
  implicit val floatDeltaReader = value[Float]
  implicit val doubleDeltaReader = value[Double]
}
