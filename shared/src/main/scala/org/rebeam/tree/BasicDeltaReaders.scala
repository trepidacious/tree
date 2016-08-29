package org.rebeam.tree

import scala.language.higherKinds

object BasicDeltaReaders {
  import DeltaReaders.deltaReaderFromReader
  implicit val stringDeltaReader = deltaReaderFromReader[String]
  implicit val booleanDeltaReader = deltaReaderFromReader[Boolean]
  implicit val byteDeltaReader = deltaReaderFromReader[Byte]
  implicit val shortDeltaReader = deltaReaderFromReader[Short]
  implicit val intDeltaReader = deltaReaderFromReader[Int]
  implicit val longDeltaReader = deltaReaderFromReader[Long]
  implicit val floatDeltaReader = deltaReaderFromReader[Float]
  implicit val doubleDeltaReader = deltaReaderFromReader[Double]
}
