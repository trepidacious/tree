package org.rebeam.tree.view

import japgolly.scalajs.react._
import monocle._
import org.rebeam.tree._
import org.rebeam.lenses._

import io.circe._

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
  def callback(delta: Delta[M], deltaJs: Json): Callback = parent.callback(delta, deltaJs)

  def act[A <: Delta[M]](actionDelta: A)(implicit encoder: Encoder[A]): Callback =
    callback(actionDelta, Json.obj("action" -> encoder(actionDelta)))

  def set(newModel: M)(implicit encoder: Encoder[M]): Callback =
    callback(ValueDelta(newModel), Json.obj("value" -> encoder(newModel)))

  //FIXME Zoomer and zoom in this form are a workaround for not being able to infer
  //type C from the lens itself in the nicer form:
  //  def zoom[C, L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
  //  Cursor[C](LensParent[M, C, L](parent, lens), lens.get(model))
  class Zoomer[C] {
    def apply[L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
      Cursor[C](LensParent[M, C, L](parent, lens), lens.get(model))
  }
  def zoom[C] = new Zoomer[C]()

  def zoomN[C](lensN: LensN[M, C]): Cursor[C] =
    Cursor[C](LensNParent(parent, lensN), lensN.get(model))

  def label(label: String) = LabelledCursor(label, this)
}

object Cursor {

  implicit class ListCursor[C](cursor: Cursor[List[C]]) {

    def zoomI(index: Int): Option[Cursor[C]] = {
      val optionalI: OptionalI[C] = OptionalI[C](index)
      optionalI.getOption(cursor.model).map { c =>
        Cursor[C](OptionalIParent(cursor.parent, optionalI), c)
      }
    }

    lazy val zoomAllI: List[Cursor[C]] = cursor.model.zipWithIndex.flatMap {
      case (a, i) => cursor.zoomI(i)
    }

    def zoomMatch[F <: C => Boolean](f: F)(implicit fEncoder: Encoder[F]): Option[Cursor[C]] = {
      val optionalMatch: OptionalMatch[C, F] = OptionalMatch[C, F](f)
      optionalMatch.getOption(cursor.model).map { c =>
        Cursor[C](OptionalMatchParent(cursor.parent, optionalMatch), c)
      }
    }

    def zoomAllMatches[F <: C => Boolean](cToF: C => F)(implicit fEncoder: Encoder[F]): List[Cursor[C]] =
      cursor.model.map(cToF).flatMap(zoomMatch(_))

  }

}

case class LabelledCursor[A](label: String, cursor: Cursor[A])
