package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import org.rebeam.lenses._
import io.circe._
import japgolly.scalajs.react.extra.Reusability
import org.rebeam.tree.ref.{Mirror, MirrorCodec}
import org.rebeam.tree.sync.{Guid, Ref => TreeRef}
import Searchable._

trait Root {
  def cursorAt[A, D <: Delta[A], L](ref: TreeRef[A], location: L)(implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, D, L]]
  def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid]
}



private case class ParentAction[M, D <: Delta[M]](parent: Parent[M, D], delta: D) extends Action {
  def callback: Callback = parent.callback(delta)
}

/**
  * Cursor giving a "position" in a data model, providing the value at that position
  * and a parent to be used to run deltas as a callback to update the entire model,
  * as well as means to zoom into the model (using lenses and similar) to produce
  * cursors for child data.
  *
  * Deltas must be of a known type.
  *
  * Also provides a Root for resolving refs
  *
  * Most commonly used by a view of a position in a model, since it provides the
  * data to display, and a way of applying deltas to the data at that position.
  *
  *
  * @tparam M     The type of model for the view.
  * @tparam D     The type of delta we can perform on the model
  * @tparam L     The type of location the cursor is viewing
  */
trait Cursor[M, D <: Delta[M], L] extends Parent[M, D] {

  /**
    * The parent of the view using this Cursor. Used to convert
    *               deltas into Callbacks that will "run" the delta.
    * @return parent
    */
  def parent: Parent[M, D]

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
  def callback(delta: D): Callback = action(delta).callback

  def action(delta: D): Action = ParentAction(parent, delta)

  def act(delta: D): Action = action(delta)

  // FIXME "set" could be provided in the case where D is a known delta type, e.  g.
  // IntValueDelta, and we could provide set(i: Int)

  def zoom[C, E <: Delta[C]](lens: LensN[M, C], childToParentDelta: E => D)(implicit s: Searchable[C, Guid]): Cursor[C, E, L] = {
    val newModel = lens.get(model)
    val newRefGuids = newModel.allRefGuids
    CursorBasic[C, E, L](ChildParent(parent, childToParentDelta), newModel, location, root, newRefGuids)
  }

  //FIXME prism

  def label(label: String): Cursor[M, D, String] = CursorBasic(parent, model, label, root, allModelRefGuids)

  def move[N](newLocation: N): Cursor[M, D, N] = CursorBasic(parent, model, newLocation, root, allModelRefGuids)

  def withoutLocation: Cursor[M, D, Unit] = move(())

  def followRef[A, E <: Delta[A]](ref: TreeRef[A])(implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, E, L]] = root.cursorAt(ref, location)

}

private case class CursorBasic[M, D <: Delta[M], L](parent: Parent[M, D], model: M, location: L, root: Root, allModelRefGuids: Set[Guid]) extends Cursor[M, D, L]

object Cursor {

  /**
    * Compare cursors
    * @tparam M   Model type
    * @tparam L   Location type
    * @return     Reusability for cursor
    */
  implicit def cursorReusability[M, D <: Delta[M], L]: Reusability[Cursor[M, D, L]] =
    Reusability.fn {
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
    def cursorAt[A, D <: Delta[A], L](ref: TreeRef[A], location: L)
                      (implicit mca: MirrorCodec[A], s: Searchable[A, Guid]): Option[Cursor[A, D, L]] = None

    override def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid] = Map.empty
  }

  def apply[M, D <: Delta[M], L](parent: Parent[M, D], model: M, location: L, root: Root)(implicit s: Searchable[M, Guid]): Cursor[M, D, L] =
    CursorBasic(parent, model, location, root, model.allRefGuids)

//  implicit class ListCursor[C, L](cursor: Cursor[List[C], L]) {
//    def zoomI(index: Int)(implicit s: Searchable[C, Guid]): Option[Cursor[C, L]] = {
//      val optionalI: OptionalI[C] = OptionalI[C](index)
//      optionalI.getOption(cursor.model).map { c =>
//        Cursor[C, L](OptionalIParent(cursor.parent, optionalI), c, cursor.location, cursor.root)
//      }
//    }
//
//    def zoomAllI(implicit s: Searchable[C, Guid]): List[Cursor[C, L]] = cursor.model.zipWithIndex.flatMap {
//      case (a, i) => cursor.zoomI(i)
//    }
//
//    def zoomMatch[F <: C => Boolean](f: F)(implicit fEncoder: Encoder[F], s: Searchable[C, Guid]): Option[Cursor[C, L]] = {
//      val optionalMatch: OptionalMatch[C, F] = OptionalMatch[C, F](f)
//      optionalMatch.getOption(cursor.model).map { c =>
//        Cursor[C, L](OptionalMatchParent(cursor.parent, optionalMatch), c, cursor.location, cursor.root)
//      }
//    }
//
//    def zoomAllMatches[F <: C => Boolean](cToF: C => F)(implicit fEncoder: Encoder[F], s: Searchable[C, Guid]): List[Cursor[C, L]] =
//      cursor.model.map(cToF).flatMap(zoomMatch(_))
//  }
//
//  implicit class OptionCursor[C, L](cursor: Cursor[Option[C], L]) {
//    def zoomOption(implicit s: Searchable[C, Guid]): Option[Cursor[C, L]] = {
//      cursor.model.map { c =>
//        Cursor[C, L](OptionParent[C](cursor.parent), c, cursor.location, cursor.root)
//      }
//    }
//  }
//
//  implicit class MirrorCursor[L](cursor: Cursor[Mirror, L]) {
//    def zoomRef[A: MirrorCodec](ref: TreeRef[A])(implicit s: Searchable[A, Guid]): Option[Cursor[A, L]] = {
//      cursor.model(ref).map { data =>
//        Cursor[A, L](MirrorParent[A](cursor.parent, ref), data, cursor.location, cursor.root)
//      }
//    }
//  }
}
