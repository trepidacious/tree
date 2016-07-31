package org.rebeam.tree

import japgolly.scalajs.react._
import monocle._
import upickle.Js
import upickle.default._

/**
  * Cursor giving a "position" in a data model, giving the model at that position
  * and a parent to be used to run deltas to update the model, as well as means to
  * zoom into the model (using a lens) to produce cursors for child data.
  *
  * Most commonly used by a view of a position in a model, since it provides the
  * data to display, and a way of applying deltas to the data at that position.
  *
  * @param parent The parent of the view using this Cursor. Used to convert
  *               deltas into Callbacks that will "run" the delta.
  * @param model  The actual model value for the child view - the data to display.
  * @tparam M     The type of model for the view.
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
