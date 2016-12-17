package org.rebeam.tree.view

import japgolly.scalajs.react._
import monocle._
import org.rebeam.tree._
import org.rebeam.lenses._

import io.circe._

trait Zoomer[C, R, M] {
  def apply[L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[R, C]
}

trait Cursor[R, M] extends Parent[M] {

  def callback(delta: Delta[M], deltaJs: Json): Callback

  def act[A <: Delta[M]](actionDelta: A)(implicit encoder: Encoder[A]): Callback

  def set(newModel: M)(implicit encoder: Encoder[M]): Callback

  //FIXME Zoomer and zoom in this form are a workaround for not being able to infer
  //type C from the lens itself in the nicer form:
  //  def zoom[C, L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
  //  Cursor[C](LensParent[M, C, L](parent, lens), lens.get(model))
  def zoom[C]: Zoomer[C, R, M]

  def zoomN[C](lensN: LensN[M, C]): Cursor[R, C]

  def label(label: String): LabelledCursor[R, M]

  def apply(): M

  def update(newModel: M)(implicit encoder: Encoder[M]): Callback

  def get: M

  def root: Cursor[R, R]

  def model: M

  def parent: Parent[M]
}

/**
  * Cursor giving a "position" in a data model, providing the value at that position
  * and a parent to be used to run deltas as a callback to update the entire model,
  * as well as means to zoom into the model (using lenses and similar) to produce
  * cursors for child data.
  *
  * Most commonly used by a view of a position in a model, since it provides the
  * data to display, and a way of applying deltas to the data at that position.
  *
  * @param parent The parent of the view using this Cursor. Used to convert
  *               deltas into Callbacks that will "run" the delta.
  * @param model  The actual model value for the child view - the data to display.
  * @tparam M     The type of model for the view.
  */
case class CursorRoot[M](parent: Parent[M], model: M) extends Cursor[M, M] {

  //Just pass through runDelta to parent for convenience
  def callback(delta: Delta[M], deltaJs: Json): Callback = parent.callback(delta, deltaJs)

  def act[A <: Delta[M]](actionDelta: A)(implicit encoder: Encoder[A]): Callback =
    callback(actionDelta, Json.obj("action" -> encoder(actionDelta)))

  def set(newModel: M)(implicit encoder: Encoder[M]): Callback =
    callback(ValueDelta(newModel), Json.obj("value" -> encoder(newModel)))

  //FIXME Zoomer and zoom in this form are a workaround for not being able to infer
  //type C from the lens itself in the nicer form:
  //  def zoom[C, L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
  //  Cursor[C](LensParent[M, C, L](parent, lens), lens.get(model))
  class ZoomerRoot[C] extends Zoomer[C, M, M]{
    def apply[L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[M, C] =
      Cursor[M, C](CursorRoot.this, LensParent[M, C, L](parent, lens), lens.get(model))
  }
  def zoom[C] = new ZoomerRoot[C]()

  def zoomN[C](lensN: LensN[M, C]): Cursor[M, C] =
    Cursor[M, C](root, LensNParent(parent, lensN), lensN.get(model))

  def label(label: String) = LabelledCursor(label, this)

  def apply(): M = model

  def update(newModel: M)(implicit encoder: Encoder[M]): Callback = set(newModel)

  def get: M = model

  def root: Cursor[M, M] = this
}

/**
  * Cursor giving a "position" in a data model, providing the value at that position
  * and a parent to be used to run deltas as a callback to update the entire model,
  * as well as means to zoom into the model (using lenses and similar) to produce
  * cursors for child data.
  *
  * Most commonly used by a view of a position in a model, since it provides the
  * data to display, and a way of applying deltas to the data at that position.
  *
  * @param parent The parent of the view using this Cursor. Used to convert
  *               deltas into Callbacks that will "run" the delta.
  * @param model  The actual model value for the child view - the data to display.
  * @tparam M     The type of model for the view.
  */
case class CursorDefault[R, M](root: Cursor[R, R], parent: Parent[M], model: M) extends Cursor[R, M] {

  //Just pass through runDelta to parent for convenience
  def callback(delta: Delta[M], deltaJs: Json): Callback = parent.callback(delta, deltaJs)

  def act[A <: Delta[M]](actionDelta: A)(implicit encoder: Encoder[A]): Callback =
    callback(actionDelta, Json.obj("action" -> encoder(actionDelta)))

  def set(newModel: M)(implicit encoder: Encoder[M]): Callback =
    callback(ValueDelta(newModel), Json.obj("value" -> encoder(newModel)))

  //FIXME Zoomer and zoom in this form are a workaround for not being able to infer
  //type C from the lens itself in the nicer form:
  //  def zoom[C, L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
  //  Cursor[C](LensParent[M, C, L](parent, lens), lens.get(model))
  class ZoomerDefault[C] extends Zoomer[C, R, M] {
    def apply[L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[R, C] =
      Cursor[R, C](root, LensParent[M, C, L](parent, lens), lens.get(model))
  }
  def zoom[C] = new ZoomerDefault[C]()

  def zoomN[C](lensN: LensN[M, C]): Cursor[R, C] =
    Cursor[R, C](root, LensNParent(parent, lensN), lensN.get(model))

  def label(label: String) = LabelledCursor(label, this)

  def apply(): M = model

  def update(newModel: M)(implicit encoder: Encoder[M]): Callback = set(newModel)

  def get: M = model
}

object Cursor {

  def apply[R, M](root: Cursor[R, R], parent: Parent[M], model: M) = CursorDefault[R, M](root, parent, model)

  def apply[R](parent: Parent[R], model: R) = CursorRoot[R](parent, model)

  implicit class ListCursor[R, C](cursor: Cursor[R, List[C]]) {

    def zoomI(index: Int): Option[Cursor[R, C]] = {
      val optionalI: OptionalI[C] = OptionalI[C](index)
      optionalI.getOption(cursor.model).map { c =>
        Cursor[R, C](cursor.root, OptionalIParent(cursor.parent, optionalI), c)
      }
    }

    lazy val zoomAllI: List[Cursor[R, C]] = cursor.model.zipWithIndex.flatMap {
      case (a, i) => cursor.zoomI(i)
    }

    def zoomMatch[F <: C => Boolean](f: F)(implicit fEncoder: Encoder[F]): Option[Cursor[R, C]] = {
      val optionalMatch: OptionalMatch[C, F] = OptionalMatch[C, F](f)
      optionalMatch.getOption(cursor.model).map { c =>
        Cursor[R, C](cursor.root, OptionalMatchParent(cursor.parent, optionalMatch), c)
      }
    }

    def zoomAllMatches[F <: C => Boolean](cToF: C => F)(implicit fEncoder: Encoder[F]): List[Cursor[R, C]] =
      cursor.model.map(cToF).flatMap(zoomMatch(_))
  }

  implicit class OptionCursor[R, C](cursor: Cursor[R, Option[C]]) {
    def zoomOption: Option[Cursor[R, C]] = {
      cursor.model.map { c =>
        Cursor[R, C](cursor.root, OptionParent[C](cursor.parent), c)
      }
    }
  }

}

case class LabelledCursor[R, A](label: String, cursor: Cursor[R, A])
