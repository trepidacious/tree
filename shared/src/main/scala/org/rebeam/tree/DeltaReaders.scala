package org.rebeam.tree

import upickle.Js
import upickle.Invalid
import upickle.default._

import scala.language.higherKinds

object DeltaReaders {
  def deltaReaderFromReader[M: Reader] = new DeltaReader[M] {
    def readDelta(deltaJs: Js.Value) = deltaJs match {
      case Js.Obj(field, _ @ _*) => field match {
        case ("value", valueJs) => ValueDelta(implicitly[Reader[M]].read(valueJs))
        case _ => throw Invalid.Data(deltaJs, "Invalid json for deltaReaderFromReader, expected object with single field name value")
      }
      case _ => throw Invalid.Data(deltaJs, "Invalid json for deltaReaderFromReader, expected object with single field name value")
    }


  }
  def deltaReaderFromPF[M](error: String)(pf: PartialFunction[Js.Value, Delta[M]]) = new DeltaReader[M] {
    def readDelta(v: Js.Value): Delta[M] = pf.applyOrElse(v, (v: Js.Value) => throw Invalid.Data(v, error))
  }
}
