package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import org.rebeam.lenses._
import io.circe._
import japgolly.scalajs.react.extra.Reusability
import org.rebeam.tree.ref.{Mirror, MirrorCodec, Ref}

trait Root {
  def cursorAt[A, L](ref: Ref[A], location: L)(implicit mca: MirrorCodec[A]): Option[Cursor[A, L]]
}

private case class ParentAction[M](parent: Parent[M], delta: Delta[M], deltaJs: Json) extends Action {
  def callback: Callback = parent.callback(delta, deltaJs)
}

// Note that these will be compared using encoder as well, but we really expect encoder to be the
// same for actions that need to be compared
private case class ActAction[M, A <: Delta[M]](parent: Parent[M], actionDelta: A, encoder: Encoder[A]) extends Action {
  def callback: Callback = parent.callback(actionDelta, Json.obj("action" -> encoder(actionDelta)))
}

// Note that these will be compared using encoder as well, but we really expect encoder to be the
// same for actions that need to be compared
private case class SetAction[M](parent: Parent[M], value: M, encoder: Encoder[M]) extends Action {
  def callback: Callback = parent.callback(ValueDelta(value), Json.obj("value" -> encoder(value)))
}

/**
  * Cursor giving a "position" in a data model, providing the value at that position
  * and a parent to be used to run deltas as a callback to update the entire model,
  * as well as means to zoom into the model (using lenses and similar) to produce
  * cursors for child data.
  *
  * Also provides a Mirror for resolving refs
  *
  * Most commonly used by a view of a position in a model, since it provides the
  * data to display, and a way of applying deltas to the data at that position.
  *
  *
  * @tparam M     The type of model for the view.
  * @tparam L     The type of location the cursor is viewing
  */
trait Cursor[M, L] extends Parent[M] {

  /**
    * The parent of the view using this Cursor. Used to convert
    *               deltas into Callbacks that will "run" the delta.
    * @return parent
    */
  def parent: Parent[M]

  /**
    * The actual model value for the child view - the data to display.
    * @return model
    */
  def model: M

  /**
    * The location the cursor is viewing
    * @return location
    */
  def location: L

  /**
    * Source for producing new cursors from a ref and location
    * @return cursor source
    */
  def root: Root

  //Just pass through callback to parent for convenience
  def callback(delta: Delta[M], deltaJs: Json): Callback = action(delta, deltaJs).callback

  def action(delta: Delta[M], deltaJs: Json): Action = ParentAction(parent, delta, deltaJs)

  def act[A <: Delta[M]](actionDelta: A)(implicit encoder: Encoder[A]): Action =
    ActAction(parent, actionDelta, encoder)
//    callback(actionDelta, Json.obj("action" -> encoder(actionDelta)))

  def set(value: M)(implicit encoder: Encoder[M]): Action =
    SetAction(parent, value, encoder)
//    callback(ValueDelta(newModel), Json.obj("value" -> encoder(newModel)))

  // FIXME Zoomer and zoom in this form are a workaround for not being able to infer
  // type C from the lens itself in the nicer form:
  //  def zoom[C, L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
  //  Cursor[C](LensParent[M, C, L](parent, lens), lens.get(model))
//  class Zoomer[C] {
//    def apply[L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
//      CursorBasic[C](LensParent[M, C, L](parent, lens), lens.get(model))
//  }
//  def zoom[C] = new Zoomer[C]()

  def zoom[C](lens: LensN[M, C]): Cursor[C, L] =
    CursorBasic[C, L](LensNParent(parent, lens), lens.get(model), location, root)

  def zoomPrism[C](prismN: PrismN[M, C]): Option[Cursor[C, L]] = {
    prismN.getOption(model).map(c => CursorBasic[C, L](PrismNParent[M, C](parent, prismN), c, location, root))
  }

  def label(label: String): Cursor[M, String] = CursorBasic(parent, model, label, root)

  def move[N](newLocation: N): Cursor[M, N] = CursorBasic(parent, model, newLocation, root)

  def withoutLocation: Cursor[M, Unit] = move(())

  def followRef[A](ref: Ref[A])(implicit mca: MirrorCodec[A]): Option[Cursor[A, L]] = root.cursorAt(ref, location)

}

private case class CursorBasic[M, L](parent: Parent[M], model: M, location: L, root: Root) extends Cursor[M, L]

object Cursor {

  object RootNone extends Root {
    override def cursorAt[A, L](ref: Ref[A], location: L)(implicit mca: MirrorCodec[A]): Option[Cursor[A, L]] = None
  }

  def apply[M, L](parent: Parent[M], model: M, location: L, root: Root): Cursor[M, L] = CursorBasic(parent, model, location, root)

  implicit class ListCursor[C, L](cursor: Cursor[List[C], L]) {

    def zoomI(index: Int): Option[Cursor[C, L]] = {
      val optionalI: OptionalI[C] = OptionalI[C](index)
      optionalI.getOption(cursor.model).map { c =>
        Cursor[C, L](OptionalIParent(cursor.parent, optionalI), c, cursor.location, cursor.root)
      }
    }

    lazy val zoomAllI: List[Cursor[C, L]] = cursor.model.zipWithIndex.flatMap {
      case (a, i) => cursor.zoomI(i)
    }

    def zoomMatch[F <: C => Boolean](f: F)(implicit fEncoder: Encoder[F]): Option[Cursor[C, L]] = {
      val optionalMatch: OptionalMatch[C, F] = OptionalMatch[C, F](f)
      optionalMatch.getOption(cursor.model).map { c =>
        Cursor[C, L](OptionalMatchParent(cursor.parent, optionalMatch), c, cursor.location, cursor.root)
      }
    }

    def zoomAllMatches[F <: C => Boolean](cToF: C => F)(implicit fEncoder: Encoder[F]): List[Cursor[C, L]] =
      cursor.model.map(cToF).flatMap(zoomMatch(_))
  }

  implicit class OptionCursor[C, L](cursor: Cursor[Option[C], L]) {
    def zoomOption: Option[Cursor[C, L]] = {
      cursor.model.map { c =>
        Cursor[C, L](OptionParent[C](cursor.parent), c, cursor.location, cursor.root)
      }
    }
  }

  implicit class MirrorCursor[L](cursor: Cursor[Mirror, L]) {
    def zoomRef[A: MirrorCodec](ref: Ref[A]): Option[Cursor[A, L]] = {
      cursor.model(ref).map { data =>
        Cursor[A, L](MirrorParent[A](cursor.parent, ref), data, cursor.location, cursor.root)
      }
    }
  }

  /**
    * Compare cursors based on parent, model and location, ignoring root
    * @tparam M   Model type
    * @tparam L   Location type
    * @return     Reusability for cursor
    */
  implicit def cursorReusability[M, L]: Reusability[Cursor[M, L]] = Reusability.fn{case (c1, c2) => c1.parent == c2.parent && c1.model == c2.model && c1.location == c2.location}

}
