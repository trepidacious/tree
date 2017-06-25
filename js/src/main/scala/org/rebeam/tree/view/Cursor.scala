package org.rebeam.tree.view

import japgolly.scalajs.react._
import monocle._
import org.rebeam.tree._
import org.rebeam.lenses._
import io.circe._
import org.rebeam.tree.ref.Cache
import org.rebeam.tree.sync.Sync.Ref

/**
  * Cursor giving a "position" in a data model, providing the value at that position
  * and a parent to be used to run deltas as a callback to update the entire model,
  * as well as means to zoom into the model (using lenses and similar) to produce
  * cursors for child data.
  *
  * Most commonly used by a view of a position in a model, since it provides the
  * data to display, and a way of applying deltas to the data at that position.
  *
  *
  * @tparam M     The type of model for the view.
  */
trait Cursor[M] extends Parent[M] {

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

  //Just pass through callback to parent for convenience
  def callback(delta: Delta[M], deltaJs: Json): Callback = parent.callback(delta, deltaJs)

  def act[A <: Delta[M]](actionDelta: A)(implicit encoder: Encoder[A]): Callback =
    callback(actionDelta, Json.obj("action" -> encoder(actionDelta)))

  def set(newModel: M)(implicit encoder: Encoder[M]): Callback =
    callback(ValueDelta(newModel), Json.obj("value" -> encoder(newModel)))

  // FIXME Zoomer and zoom in this form are a workaround for not being able to infer
  // type C from the lens itself in the nicer form:
  //  def zoom[C, L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
  //  Cursor[C](LensParent[M, C, L](parent, lens), lens.get(model))
  class Zoomer[C] {
    def apply[L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
      CursorBasic[C](LensParent[M, C, L](parent, lens), lens.get(model))
  }
  def zoom[C] = new Zoomer[C]()

  def zoomN[C](lensN: LensN[M, C]): Cursor[C] =
    CursorBasic[C](LensNParent(parent, lensN), lensN.get(model))

  def zoomPrismN[C](prismN: PrismN[M, C]): Option[Cursor[C]] = {
    prismN.getOption(model).map(c => CursorBasic[C](PrismNParent[M, C](parent, prismN), c))
  }

  def label(label: String) = CursorL(parent, model, label)

  def withP[P](p: P): CursorP[M, P] = CursorP(parent, model, p)
}

private case class CursorBasic[M](parent: Parent[M], model: M) extends Cursor[M]

object Cursor {

  def apply[M](parent: Parent[M], model: M): Cursor[M] = CursorBasic(parent, model)

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

  implicit class OptionCursor[C](cursor: Cursor[Option[C]]) {
    def zoomOption: Option[Cursor[C]] = {
      cursor.model.map { c =>
        Cursor[C](OptionParent[C](cursor.parent), c)
      }
    }
  }

  implicit class CacheCursor[A](cursor: Cursor[Cache[A]]) {
    def zoomRef(ref: Ref[A]): Option[Cursor[A]] = {
      cursor.model(ref).map { data =>
        Cursor[A](CacheParent[A](cursor.parent, OptionalCache(ref)), data)
      }
    }

    def zoomRefPrism[B <: A](ref: Ref[B])(implicit p: PrismN[A, B]): Option[Cursor[B]] = {
      zoomRef(ref).flatMap(c => c.zoomPrismN[B](p))
    }
  }

  implicit class ListCursorP[C, P](cursor: CursorP[List[C], P]) {

    def zoomIP(index: Int): Option[CursorP[C, P]] = {
      val optionalI: OptionalI[C] = OptionalI[C](index)
      optionalI.getOption(cursor.model).map { c =>
        CursorP[C, P](OptionalIParent(cursor.parent, optionalI), c, cursor.p)
      }
    }

    lazy val zoomAllIP: List[CursorP[C, P]] = cursor.model.zipWithIndex.flatMap {
      case (_, i) => cursor.zoomIP(i)
    }

    def zoomMatchP[F <: C => Boolean](f: F)(implicit fEncoder: Encoder[F]): Option[CursorP[C, P]] = {
      val optionalMatch: OptionalMatch[C, F] = OptionalMatch[C, F](f)
      optionalMatch.getOption(cursor.model).map { c =>
        CursorP[C, P](OptionalMatchParent(cursor.parent, optionalMatch), c, cursor.p)
      }
    }

    def zoomAllMatchesP[F <: C => Boolean](cToF: C => F)(implicit fEncoder: Encoder[F]): List[CursorP[C, P]] =
      cursor.model.map(cToF).flatMap(zoomMatchP(_))
  }

  implicit class OptionCursorP[C, P](cursor: CursorP[Option[C], P]) {
    def zoomOptionP: Option[CursorP[C, P]] = {
      cursor.model.map { c =>
        CursorP[C, P](OptionParent[C](cursor.parent), c, cursor.p)
      }
    }
  }

}

case class CursorL[A](parent: Parent[A], model: A, label: String) extends Cursor[A]

case class CursorP[A, P](parent: Parent[A], model: A, p: P) extends Cursor[A] {

  // FIXME Zoomer and zoom in this form are a workaround for not being able to infer
  // type C from the lens itself in the nicer form:
  //  def zoom[C, L <: Lens[M, C]: OuterEncoder](lens: L): Cursor[C] =
  //  Cursor[C](LensParent[M, C, L](parent, lens), lens.get(model))
  class ZoomerP[C] {
    def apply[L <: Lens[A, C]: OuterEncoder](lens: L): Cursor[C] =
      CursorP[C, P](LensParent[A, C, L](parent, lens), lens.get(model), p)
  }
  def zoomP[C] = new ZoomerP[C]()

  def zoomNP[C](lensN: LensN[A, C]): CursorP[C, P] =
    CursorP[C, P](LensNParent(parent, lensN), lensN.get(model), p)

  def zoomPrismNP[C](prismN: PrismN[A, C]): Option[CursorP[C, P]] = {
    prismN.getOption(model).map(c => CursorP[C, P](PrismNParent(parent, prismN), c, p))
  }
}
