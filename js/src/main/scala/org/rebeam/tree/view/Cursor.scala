package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import org.rebeam.lenses._
import io.circe._
import japgolly.scalajs.react.extra.Reusability
import org.rebeam.tree.DeltaCodecs.DeltaCodec
import org.rebeam.tree.ref.Cache
import org.rebeam.tree.ref.Ref

/**
  * A value, and a parent used to produce callbacks to run deltas on that value
  * @tparam C The type of data in the cache
  * @tparam A The type of value
  */
trait ValueAndParent[C, A] {
  /**
    * Used to convert deltas to the value into Callbacks that will run the delta.
    * @return parent
    */
  def parent: Parent[C, A]

  /**
    * The value
    * @return value
    */
  def value: A
}

case class ValueAndParentBasic[C, A](value: A, parent: Parent[C, A]) extends ValueAndParent[C, A]

object ValueAndParent {
  def apply[C, A](value: A, parent: Parent[C, A]): ValueAndParentBasic[C, A] = ValueAndParentBasic[C, A](value, parent)
}

/**
  * Cursor for a value within a data model, with access to a Cache via a ValueAndParent,
  * and a location within that model.
  * Is also itself a ValueAndParent.
  * @tparam A Type of data
  * @tparam C Type of data in the Cache
  * @tparam L Type of location
  */
trait Cursor[A, C, L] extends ValueAndParent[C, A] {

  /**
    * The location to display. This might be something like a path through
    * the data, or a label for the data. Often provided by a Router.
    * @return location
    */
  def location: L

  /**
    * A ValueAndParent for the cache
    * @return cacheVAP
    */
  def cacheVAP: ValueAndParent[C, Cache[C]]

  /**
    * The cache in which to look up references
    * @return cache
    */
  def cache: Cache[C] = cacheVAP.value

  //Just pass through callback to parent for convenience
  def callback(delta: Delta[C, A], deltaJs: Json): Callback = parent.callback(delta, deltaJs)

  /**
    * Produce a callback for applying an action Delta on the cursor
    * @param actionDelta  The Delta to apply as an action
    * @param encoder      An encoder for the action
    * @tparam D           The type of action Delta
    * @return             A Callback that will apply the action
    */
  def act[D <: Delta[C, A]](actionDelta: D)(implicit encoder: Encoder[D]): Callback =
    callback(actionDelta, Json.obj("action" -> encoder(actionDelta)))

  /**
    * Produce a callback for setting a new value
    * @param newValue The new value to set
    * @param encoder  An encoder for the value
    * @return         A Callback that will set the value
    */
  def set(newValue: A)(implicit encoder: Encoder[A]): Callback =
    callback(ValueDelta(newValue), Json.obj("value" -> encoder(newValue)))

  // Zoom methods pass from the current value to some contained value, reached by a lens, prism etc.

  /**
    * Zoom the cursor into a contained value, reached from the current value using a LensN
    * @param lensN    The LensN used to reach the new value from the current value
    * @tparam B       The type of value in the new cursor
    * @return         A new cursor
    */
  def zoom[B](lensN: LensN[A, B]): Cursor[B, C, L] =
    CursorBasic[B, C, L](LensNParent(parent, lensN), lensN.get(value), location, cacheVAP)

  /**
    * Zoom the cursor into a contained value, reached from the current value using a PrismN
    * @param prismN   The PrismN used to reach the new value from the current value
    * @tparam B       The type of value in the new cursor
    * @return         A new cursor, or None if the PrismN returns no value
    */
  def zoomPrism[B](implicit prismN: PrismN[A, B]): Option[Cursor[B, C, L]] = {
    prismN.getOption(value).map(c => CursorBasic[B, C, L](PrismNParent[C, A, B](parent, prismN), c, location, cacheVAP))
  }

  // Follow methods return to the cache, and look up a reference, to create a new cursor at the root of the
  // referenced data

  /**
    * Use the cache to look up a data item from a reference. Produce a new cursor with this data as its value,
    * with the same location and cache.
    * @param ref    The reference to follow
    * @return       A new cursor pointing at the referenced data, or None if that data is not in the cache
    */
  def followRef(ref: Ref[C]): Option[Cursor[C, C, L]] = {
    cacheVAP.value(ref).map { data =>
      CursorBasic[C, C, L](CacheParent[C, C](cacheVAP.parent, OptionalCache(ref)), data, location, cacheVAP)
    }
  }

  /**
    * Use the cache to look up a data item from a reference, and then zoom into it using a prism.
    * Produce a new cursor with this data as its value, with the same location and cache.
    * This is equivalent to followRef then zoomPrism, but may be more concise.
    * @param ref    The reference to follow
    * @param prism  The prism to apply to the data retrieved from the Cache
    * @return       A new cursor pointing at the referenced data, or None if that data is not in the cache
    */
  def followRefPrism[D <: C](ref: Ref[D])(implicit prism: PrismN[C, D]): Option[Cursor[D, C, L]] = followRef(ref).flatMap(_.zoomPrism(prism))

  // The following methods affect the location

  /**
    * Set the location to a String label
    * @param label  The label
    * @return       A new Cursor with the label as its location
    */
  def label(label: String): Cursor[A, C, String] = move(label)

  /**
    * Set a new location
    * @param newLocation  The new location to move to
    * @return             A new Cursor with the new location
    */
  def move[M](newLocation: M): Cursor[A, C, M] = CursorBasic(parent, value, newLocation, cacheVAP)

}

private case class CursorBasic[A, C, L](parent: Parent[C, A], value: A, location: L, cacheVAP: ValueAndParent[C, Cache[C]]) extends Cursor[A, C, L]

object Cursor {

  implicit def cursorReusability[A, C, L]: Reusability[Cursor[A, C, L]] = Reusability.fn{case (a, b) => a.parent == b.parent && a.value == b.value && a.location == b.location}

  def apply[A, C, L](parent: Parent[C, A], value: A, location: L, cacheVAP: ValueAndParent[C, Cache[C]]): Cursor[A, C, L] = CursorBasic(parent, value, location, cacheVAP)

  def emptyCacheVAP[C](implicit codec: DeltaCodec[C, C]): ValueAndParent[C, Cache[C]] = ValueAndParent(Cache.empty[C], new Parent[C, Cache[C]] {
    override def callback(delta: Delta[C, Cache[C]], deltaJs: Json): Callback = Callback.empty
  })

  implicit class ListCursor[A, C, L](cursor: Cursor[List[A], C, L]) {

    def zoomI(index: Int): Option[Cursor[A, C, L]] = {
      val optionalI: OptionalI[A] = OptionalI[A](index)
      optionalI.getOption(cursor.value).map { a =>
        CursorBasic[A, C, L](OptionalIParent(cursor.parent, optionalI), a, cursor.location, cursor.cacheVAP)
      }
    }

    lazy val zoomAllI: List[Cursor[A, C, L]] = cursor.value.indices.toList.flatMap(cursor.zoomI(_))

    def zoomMatch[F <: A => Boolean](f: F)(implicit fEncoder: Encoder[F]): Option[Cursor[A, C, L]] = {
      val optionalMatch: OptionalMatch[A, F] = OptionalMatch[A, F](f)
      optionalMatch.getOption(cursor.value).map { a =>
        CursorBasic[A, C, L](OptionalMatchParent(cursor.parent, optionalMatch), a, cursor.location, cursor.cacheVAP)
      }
    }

    def zoomAllMatches[F <: A => Boolean](aToF: A => F)(implicit fEncoder: Encoder[F]): List[Cursor[A, C, L]] =
      cursor.value.map(aToF).flatMap(zoomMatch(_))
  }

  implicit class OptionCursor[A, C, L](cursor: Cursor[Option[A], C, L]) {
    def zoomOption: Option[Cursor[A, C, L]] = {
      cursor.value.map { a =>
        CursorBasic[A, C, L](OptionParent[C, A](cursor.parent), a, cursor.location, cursor.cacheVAP)
      }
    }
  }

}
