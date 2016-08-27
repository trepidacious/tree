package org.rebeam.tree

import monocle._
import upickle.{Invalid, Js}
import upickle.default._

object DeltaReader {

  class Builder[M: Reader](fToD: PartialFunction[(String, Js.Value), Delta[M]]) extends DeltaReader[M] {
    def readDelta(deltaJs: Js.Value): Delta[M] = deltaJs match {
      case Js.Obj(deltaField, _ @ _*) => deltaField match {

        case ("lens", lensJs) => lensJs match {
          //Use our fieldToDelta - if it can't convert to a delta, invalid data
          case Js.Obj(lensField, _ @ _*) => fToD.applyOrElse(lensField, (f: (String, Js.Value)) => throw Invalid.Data(lensJs, "Unknown field " + f + " in lens delta object " + lensJs))
          case _ => throw Invalid.Data(lensJs, "Invalid delta, empty object for lens delta")
        }

        case ("value", valueJs) => ValueDelta(implicitly[Reader[M]].read(valueJs))

        case _ => throw Invalid.Data(deltaJs, "Invalid delta, expected object with field lens, set or action")
      }
      case _ => throw Invalid.Data(deltaJs, "Invalid delta, expected object")
    }

    /**
      * Add a field to delta
      */
    def fieldToDelta(newFToD: PartialFunction[(String, Js.Value), Delta[M]]) =
    new Builder(fToD.orElse(newFToD))

    /**
      * Add a lens from M to S
      */
    def lens[S: DeltaReader](fieldName: String, theLens: Lens[M, S]) = fieldToDelta {
      case (fn, v) if fn == fieldName => LensDelta(theLens, implicitly[DeltaReader[S]].readDelta(v))
    }

    def action[A <: Delta[M] : Reader] = new DeltaReaderWithAction(this)
  }

  class DeltaReaderWithAction[M, A <: Delta[M] : Reader](delegate: DeltaReader[M]) extends DeltaReader[M] {
    def readDelta(deltaJs: Js.Value): Delta[M] = deltaJs match {
      case Js.Obj(field, _ @ _*) => field match {
        case ("action", actionJs) => implicitly[Reader[A]].read(actionJs)

        case _ => delegate.readDelta(deltaJs)
      }
      case _ => throw Invalid.Data(deltaJs, "Invalid delta, expected object")
    }
  }

  def build[M: Reader] = new Builder[M](PartialFunction.empty)
}

trait DeltaReader[M] {
  def readDelta(v: Js.Value): Delta[M]
}