package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import io.circe._
import japgolly.scalajs.react.extra.Reusability
import org.rebeam.tree.ref.{Mirror, MirrorCodec}
import org.rebeam.tree.sync.{Guid, Ref => TreeRef}
import Searchable._
import monocle.{Lens, Prism}

trait Root {
  def cursorAt[A, L](ref: TreeRef[A], location: L)(implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, L]]
  def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid]
}

private case class ParentAction[M](parent: Parent[M], delta: Delta[M]) extends Action {
  def callback: Callback = parent.callback(delta)
}

private case class ActAction[M, A <: Delta[M]](parent: Parent[M], actionDelta: A) extends Action {
  def callback: Callback = parent.callback(actionDelta)
}

private case class SetAction[M](parent: Parent[M], value: M) extends Action {
  def callback: Callback = parent.callback(ValueDelta(value))
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

  /**
    * Produce the Guids of all Refs in the model
    * @return Set of all Ref Guids in the model
    */
  def allModelRefGuids: Set[Guid]

  //Just pass through callback to parent for convenience
  def callback(delta: Delta[M]): Callback = action(delta).callback

  def action(delta: Delta[M]): Action = ParentAction(parent, delta)

  def act[A <: Delta[M]](actionDelta: A): Action = ActAction(parent, actionDelta)
//    callback(actionDelta, Json.obj("action" -> encoder(actionDelta)))

  def set(value: M): Action = SetAction(parent, value)
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

  def zoom[C](lens: Lens[M, C])(implicit s: Searchable[C, Guid]): Cursor[C, L] = {
    val newModel = lens.get(model)
    val newRefGuids = newModel.allRefGuids
    CursorBasic[C, L](LensParent(parent, lens), newModel, location, root, newRefGuids)
  }

  def zoomPrism[C](prism: Prism[M, C]): Option[Cursor[C, L]] = {
    prism.getOption(model).map(c => CursorBasic[C, L](PrismParent[M, C](parent, prism), c, location, root, allModelRefGuids))
  }

  def label(label: String): Cursor[M, String] = CursorBasic(parent, model, label, root, allModelRefGuids)

  def move[N](newLocation: N): Cursor[M, N] = CursorBasic(parent, model, newLocation, root, allModelRefGuids)

  def withoutLocation: Cursor[M, Unit] = move(())

  def followRef[A](ref: TreeRef[A])(implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, L]] = root.cursorAt(ref, location)

}

private case class CursorBasic[M, L](parent: Parent[M], model: M, location: L, root: Root, allModelRefGuids: Set[Guid]) extends Cursor[M, L]

object Cursor {

  /**
    * Compare cursors
    * @tparam M   Model type
    * @tparam L   Location type
    * @return     Reusability for cursor
    */
  implicit def cursorReusability[M, L]: Reusability[Cursor[M, L]] =
    Reusability {
      case (c1, c2) =>
        // First check for changes in parent, model or location (i.e. everything except
        // changes to the data pointed to by Refs)
        if (!(c1.parent == c2.parent && c1.model == c2.model && c1.location == c2.location)) {
//          println("c1 != c2 (non-ref) " + c1 + ", c2")
          false

        // Otherwise look for changes in referenced values.
        } else {
          // We have changed if the revision of any referenced data has
          // changed between c1.root and c2.root
          val c1RefRevs = c1.root.refRevisions(c1.allModelRefGuids)
          val c2RefRevs = c2.root.refRevisions(c2.allModelRefGuids)
          val e = c1RefRevs == c2RefRevs
//          println("c1 " + c1 + ", c1RefRevs " + c1RefRevs + ", c2Refs " + c2RefRevs + ", equal? " + e)
          e
        }
    }

  object RootNone extends Root {
    def cursorAt[A, L](ref: TreeRef[A], location: L)
                      (implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, L]] = None
    override def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid] = Map.empty
  }

  def apply[M, L](parent: Parent[M], model: M, location: L, root: Root)(implicit s: Searchable[M, Guid]): Cursor[M, L] =
    CursorBasic(parent, model, location, root, model.allRefGuids)

  //FIXME add a ListIdCursor to provide for zooming by id
  implicit class ListCursor[C, L](cursor: Cursor[List[C], L]) {
    def zoomI(index: Int)(implicit s: Searchable[C, Guid]): Option[Cursor[C, L]] = {
      val optionalI: OptionalI[C] = OptionalI[C](index)
      optionalI.getOption(cursor.model).map { c =>
        Cursor[C, L](OptionalIParent(cursor.parent, optionalI), c, cursor.location, cursor.root)
      }
    }

    def zoomAllI(implicit s: Searchable[C, Guid]): List[Cursor[C, L]] = cursor.model.zipWithIndex.flatMap {
      case (a, i) => cursor.zoomI(i)
    }

    def zoomMatch[F <: C => Boolean](f: F)(implicit fEncoder: Encoder[F], s: Searchable[C, Guid]): Option[Cursor[C, L]] = {
      val optionalMatch: OptionalMatch[C, F] = OptionalMatch[C, F](f)
      optionalMatch.getOption(cursor.model).map { c =>
        Cursor[C, L](OptionalMatchParent(cursor.parent, optionalMatch), c, cursor.location, cursor.root)
      }
    }

    def zoomAllMatches[F <: C => Boolean](cToF: C => F)(implicit fEncoder: Encoder[F], s: Searchable[C, Guid]): List[Cursor[C, L]] =
      cursor.model.map(cToF).flatMap(zoomMatch(_))
  }

  implicit class OptionCursor[C, L](cursor: Cursor[Option[C], L]) {
    def zoomOption(implicit s: Searchable[C, Guid]): Option[Cursor[C, L]] = {
      cursor.model.map { c =>
        Cursor[C, L](OptionParent[C](cursor.parent), c, cursor.location, cursor.root)
      }
    }
  }

  implicit class MirrorCursor[L](cursor: Cursor[Mirror, L]) {
    def zoomRef[A: MirrorCodec](ref: TreeRef[A])(implicit s: Searchable[A, Guid]): Option[Cursor[A, L]] = {
      cursor.model(ref).map { data =>
        Cursor[A, L](MirrorParent[A](cursor.parent, ref), data, cursor.location, cursor.root)
      }
    }
  }
}
