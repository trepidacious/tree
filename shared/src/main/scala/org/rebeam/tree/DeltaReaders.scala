package org.rebeam.tree

import upickle.Js
import upickle.Invalid
import upickle.default._

import scala.language.higherKinds

object DeltaReaders {
  def deltaReaderFromReader[M: Reader] = new DeltaReader[M] {
    def readDelta(v: Js.Value) = ValueDelta(implicitly[Reader[M]].read(v))
  }
  def deltaReaderFromPF[M](error: String)(pf: PartialFunction[Js.Value, Delta[M]]) = new DeltaReader[M] {
    def readDelta(v: Js.Value): Delta[M] = pf.applyOrElse(v, (v: Js.Value) => throw Invalid.Data(v, error))
  }
  implicit val stringDeltaReader = deltaReaderFromReader[String]
  implicit val booleanDeltaReader = deltaReaderFromReader[Boolean]
  implicit val byteDeltaReader = deltaReaderFromReader[Byte]
  implicit val shortDeltaReader = deltaReaderFromReader[Short]
  implicit val intDeltaReader = deltaReaderFromReader[Int]
  implicit val longDeltaReader = deltaReaderFromReader[Long]
  implicit val floatDeltaReader = deltaReaderFromReader[Float]
  implicit val doubleDeltaReader = deltaReaderFromReader[Double]
}
