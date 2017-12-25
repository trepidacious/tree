package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import japgolly.scalajs.react.extra.Reusability
import org.rebeam.tree.sync.{Guid, Ref => TreeRef}
import Searchable._

trait Root {
  def cursorAt[U, A, D <: Delta[U, A], L](ref: TreeRef[A], location: L)(implicit s: Searchable[A, Guid]): Option[Cursor[U, A, D, L]]
  def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid]
}

private case class ParentAction[U, M, D <: Delta[U, M]](parent: Parent[U, M, D], delta: D) extends Action {
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
  * @tparam U     The type of data accessible via reference.
  * @tparam M     The type of model for the view.
  * @tparam D     The type of delta we can perform on the model
  * @tparam L     The type of location the cursor is viewing
  */
trait Cursor[U, M, D <: Delta[U, M], L] extends Parent[U, M, D] {

  /**
    * The parent of the view using this Cursor. Used to convert
    *               deltas into Callbacks that will "run" the delta.
    * @return parent
    */
  def parent: Parent[U, M, D]

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

  def zoom[C, E <: Delta[U, C]](dLens: DLens[U, M, D, C, E])(implicit s: Searchable[C, Guid]): Cursor[U, C, E, L] = {
    val newModel = dLens.aToB(model)
    val newRefGuids = newModel.allRefGuids
    CursorBasic[U, C, E, L](DLensParent(parent, dLens), newModel, location, root, newRefGuids)
  }

  def zoomOptional[C, E <: Delta[U, C]](dOptional: DOptional[U, M, D, C, E])(implicit s: Searchable[C, Guid]): Option[Cursor[U, C, E, L]] = {
    dOptional.aToB(model).map {
      newModel =>
        val newRefGuids = newModel.allRefGuids
        CursorBasic[U, C, E, L](DOptionalParent(parent, dOptional), newModel, location, root, newRefGuids)
    }
  }

  //FIXME add zoomPrism if needed

  def label(label: String): Cursor[U, M, D, String] = CursorBasic(parent, model, label, root, allModelRefGuids)
  def move[N](newLocation: N): Cursor[U, M, D, N] = CursorBasic(parent, model, newLocation, root, allModelRefGuids)
  def withoutLocation: Cursor[U, M, D, Unit] = move(())
  def followRef[A, E <: Delta[U, A]](ref: TreeRef[A])(implicit s: Searchable[A, Guid]): Option[Cursor[U, A, E, L]] = root.cursorAt(ref, location)
}

private case class CursorBasic[U, M, D <: Delta[U, M], L](parent: Parent[U, M, D], model: M, location: L, root: Root, allModelRefGuids: Set[Guid]) extends Cursor[U, M, D, L]

object Cursor {

  /**
    * Compare cursors
    * @tparam M   Model type
    * @tparam L   Location type
    * @return     Reusability for cursor
    */
  implicit def cursorReusability[U, M, D <: Delta[U, M], L]: Reusability[Cursor[U, M, D, L]] =
    Reusability.fn {
      case (c1, c2) =>
        // First check for changes in parent, model or location (i.e. everything except
        // changes to the data pointed to by Refs)
        if (!(c1.parent == c2.parent && c1.model == c2.model && c1.location == c2.location)) {
//          println(s"c1 != c2 (non-ref) $c1, $c2, parent equal? ${c1.parent == c2.parent}, model equal? ${c1.model == c2.model}, location equal? ${c1.location == c2.location}" )
          false

        // Otherwise look for changes in referenced values.
        } else {
          // We have changed if the revision of any referenced data has
          // changed between c1.root and c2.root
          val c1RefRevs = c1.root.refRevisions(c1.allModelRefGuids)
          val c2RefRevs = c2.root.refRevisions(c2.allModelRefGuids)
          val e = c1RefRevs == c2RefRevs
//          println("c1 " + c1 + ", c1RefRevs " + c1RefRevs + ", c2RefRevs " + c2RefRevs + ", equal? " + e)
          e
        }
    }

  object RootNone extends Root {
    def cursorAt[U, A, D <: Delta[U, A], L](ref: TreeRef[A], location: L)
                      (implicit s: Searchable[A, Guid]): Option[Cursor[U, A, D, L]] = None

    override def refRevisions(refGuids: Set[Guid]): Map[Guid, Guid] = Map.empty
  }

  def apply[U, M, D <: Delta[U, M], L](parent: Parent[U, M, D], model: M, location: L, root: Root)(implicit s: Searchable[M, Guid]): Cursor[U, M, D, L] =
    CursorBasic(parent, model, location, root, model.allRefGuids)

  implicit class ListIndexCursor[U, A, D <: Delta[U, A], L](cursor: Cursor[U, List[A], ListIndexDelta[U, A, D], L])(implicit s: Searchable[A, Guid]) {
    def zoomI(i: Int): Option[Cursor[U, A, D, L]] = cursor.zoomOptional(ListIndexDelta.toIndex(i))
    def zoomAllI: List[Cursor[U, A, D, L]] = cursor.model.indices.flatMap(cursor.zoomI).toList
  }

  implicit class ListMatchCursor[U, A, D <: Delta[U, A], L, F <: A => Boolean](cursor: Cursor[U, List[A], ListMatchDelta[U, A, D, F], L])(implicit s: Searchable[A, Guid]) {
    def zoomMatch(f: F): Option[Cursor[U, A, D, L]] = cursor.zoomOptional(ListMatchDelta.toMatch(f))
    def zoomAllMatches(toFinder: A => F): List[Cursor[U, A, D, L]] = cursor.model.map(toFinder).flatMap(cursor.zoomMatch)
  }

  implicit class OptionCursor[U, A, D <: Delta[U, A], L](cursor: Cursor[U, Option[A], OptionDelta[U, A, D], L])(implicit s: Searchable[A, Guid]) {
    def zoomOption: Option[Cursor[U, A, D, L]] = cursor.zoomOptional(OptionDelta.toSome[U, A, D])
  }

  implicit class ValueCursor[U, A, L](cursor: Cursor[U, A, ValueDelta[U, A], L])(implicit s: Searchable[A, Guid]) {
    def set(a: A): Action = cursor.act(ValueDelta(a))
  }

//  implicit class MirrorCursor[L](cursor: Cursor[Mirror, L]) {
//    def zoomRef[A: MirrorCodec](ref: TreeRef[A])(implicit s: Searchable[A, Guid]): Option[Cursor[A, L]] = {
//      cursor.model(ref).map { data =>
//        Cursor[A, L](MirrorParent[A](cursor.parent, ref), data, cursor.location, cursor.root)
//      }
//    }
//  }

}
