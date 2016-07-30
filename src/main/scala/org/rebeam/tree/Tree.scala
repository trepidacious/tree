package org.rebeam.tree

import japgolly.scalajs.react.Callback
import monocle._
import upickle.Js
import upickle.Invalid
import upickle.default._

import scala.language.higherKinds

object DeltaReaders {
  def deltaReaderFromReader[M: Reader] = new DeltaReader[M] {
    def readDelta(v: Js.Value) = ValueDelta(implicitly[Reader[M]].read(v))
  }
  def deltaReaderFromPF[M](error: String)(pf: PartialFunction[Js.Value, Delta[M]]) = new DeltaReader[M] {
    def readDelta(v: Js.Value): Delta[M] = pf.applyOrElse(v, (v: Js.Value) => throw new Invalid.Data(v, error))
  }
  implicit val stringDeltaReader = deltaReaderFromReader[String]
  implicit val intDeltaReader = deltaReaderFromReader[Int]
}

object DeltaReader {
  def build[M: Reader] = new DeltaReaderBuilder[M](PartialFunction.empty)
}

class DeltaReaderBuilder[M: Reader](fToD: PartialFunction[(String, Js.Value), Delta[M]]) extends DeltaReader[M] {
  def readDelta(v: Js.Value): Delta[M] = v match {
    case Js.Obj(field, _ @ _*) => field match {
      
      case ("lens", v) => v match {
        //Use our fieldToDelta - if it can't convert to a delta, invalid data
        case Js.Obj(field, _ @ _*) => fToD.applyOrElse(field, (field: (String, Js.Value)) => throw new Invalid.Data(v, "Unknown field " + field + " in lens delta object " + v))
        case _ => throw new Invalid.Data(v, "Invalid delta, empty object for lens delta")
      }

      case ("value", v) => ValueDelta(implicitly[Reader[M]].read(v))
      
      case _ => throw new Invalid.Data(v, "Invalid delta, expected object with field lens, set or action")
    }
    case _ => throw new Invalid.Data(v, "Invalid delta, expected object")
  }
  
  /**
    * Add a field to delta
    */
  def fieldToDelta(newFToD: PartialFunction[(String, Js.Value), Delta[M]]) = 
    new DeltaReaderBuilder(fToD.orElse(newFToD))
    
  /**
    * Add a lens from M to S
    */
  def lens[S: DeltaReader](fieldName: String, theLens: Lens[M, S]) = fieldToDelta {
    case (fieldName, v) => LensDelta(theLens, implicitly[DeltaReader[S]].readDelta(v))
  }
  
  def action[A <: Delta[M] : Reader] = new DeltaReaderWithAction(this)
}

object DeltaReaderBuilder {
  def empty[M: Reader] = new DeltaReaderBuilder[M](PartialFunction.empty)
}

class DeltaReaderWithAction[M, A <: Delta[M] : Reader](delegate: DeltaReader[M]) extends DeltaReader[M] {
  def readDelta(v: Js.Value): Delta[M] = v match {
    case Js.Obj(field, _ @ _*) => field match {
      case ("action", v) => implicitly[Reader[A]].read(v)
      
      case _ => delegate.readDelta(v) 
    }
    case _ => throw new Invalid.Data(v, "Invalid delta, expected object")
  }
}

/**
 * Interface provided by a parent component to a child component, 
 * allowing it to run a delta, which is also required encoded to JSON.
 * This is all the child component needs to know about a parent component.
 * The child's model itself (of type C) will be passed separately.
 */
trait Parent[C] {
  def callback(delta: Delta[C], deltaJs: Js.Value): Callback
}

/**
 * The parent at the root of a model of type R. This handles deltas by 
 * using them to produce a callback. This can be used by a view of the root of a model
 * to produce Parent instances for its children using Parent implementations
 * that expect a Parent themselves.
 */
case class RootParent[R](deltaToCallback: (Delta[R], Js.Value) => Callback) extends Parent[R] {
  def callback(delta: Delta[R], deltaJs: Js.Value): Callback = deltaToCallback(delta, deltaJs)
}

/**
 * Produce a parent for a child component, using a lens for a named field to
 * reach that child's model.
 */ 
case class LensParent[P, C](parent: Parent[P], fieldName: String, lens: Lens[P, C]) extends Parent[C] {
  def callback(delta: Delta[C], deltaJs: Js.Value): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = LensDelta(lens, delta)
    
    //Add this delta to the JSON
    val parentDeltaJs = Js.Obj("lens" -> Js.Obj(fieldName -> deltaJs))
    
    //Run using the parent's own parent
    parent.callback(parentDelta, parentDeltaJs)
  }
}

/**
 * Cursor into a root data model, giving the model at the current location and
 * a parent to be used to run deltas to update the model, as well as means to
 * zoom into the model to produce cursors for child data
 */
case class Cursor[M](parent: Parent[M], model: M) extends Parent[M] {
  
  //Just pass through runDelta to parent for convenience
  def callback(delta: Delta[M], deltaJs: Js.Value): Callback = parent.callback(delta, deltaJs)
  
  def act[A <: Delta[M]](actionDelta: A)(implicit writer: Writer[A]): Callback =
    callback(actionDelta, Js.Obj("action" -> writer.write(actionDelta)))

  def set(newModel: M)(implicit writer: Writer[M]): Callback =
    callback(ValueDelta(newModel), Js.Obj("value" -> writer.write(newModel)))
  
  //TODO if we had a FieldLens being a Lens with an added fieldName: String we could use this instead, and
  //use a macro to provide these (and readDelta implementation)
  //TODO macros could generate a specific cursor type (e.g. AddressCursor) for each model type, having
  //methods to zoom to the cursor for each field, providing the appropriate child cursor
  //type for that child (e.g. StreetCursor), when that child is also using the same macro?
  //This would then prevent use of invalid fields, and could propagate access control through
  //a data model, etc.
  def zoom[C](fieldName: String, lens: Lens[M, C]): Cursor[C] = 
    Cursor(LensParent(parent, fieldName, lens), lens.get(model))
}

trait Delta[M] {
  def apply(m: M): M
}

case class LensDelta[A, B](lens: Lens[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): A = lens.modify(delta.apply)(a)
}

case class OptionalDelta[A, B](optional: Optional[A, B], delta: Delta[B]) extends Delta[A] {
  def apply(a: A): A = optional.modify(delta.apply)(a)
}

case class ValueDelta[M](v: M) extends Delta[M] {
  def apply(m: M): M = v
}

trait DeltaReader[M] {
  def readDelta(v: Js.Value): Delta[M]
}

