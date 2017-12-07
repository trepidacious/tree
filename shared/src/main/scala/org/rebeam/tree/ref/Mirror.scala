package org.rebeam.tree.ref

import io.circe._
import org.rebeam.tree.Delta._
import org.rebeam.tree.{Delta, Searchable}
import org.rebeam.tree.sync._

object Mirror {
  def empty[U, A, D <: Delta[U, A]]: Mirror[U, A, D] = new Mirror[U, A, D](Map.empty)

  // There is no meaning to searching for refs in a mirror
  implicit def mirrorNotSearchableForGuid[U, A, D <: Delta[U, A]]: Searchable[Mirror[U, A, D], Guid] = Searchable.notSearchable

  /**
    * Decoder for mirrors containing only the types handled by the provided MirrorCodecs
    * @param decoder  Decoder for data contents
    * @return         A Decoder[Mirror[U, A, D]
    */
  def decoder[U, A, D <: Delta[U, A]](implicit decoder: Decoder[A]): Decoder[Mirror[U, A, D]] = {
    Decoder.instance[Mirror[U, A, D]](
      // FIXME implement, use contramap or similar based on map?
      c => Left[DecodingFailure, Mirror[U, A, D]](DecodingFailure("No fields in mirror Json", c.history))
    )
  }


  /**
    * Encode as an object with fields from guids.
    * Each field contains an object named after the MirrorType of the data item,
    * containing its revision and data.
    */
  def encoder[U, A, D <: Delta[U, A]](implicit encoder: Encoder[A]): Encoder[Mirror[U, A, D]] = Encoder.instance[Mirror[U, A, D]](
    m => {
      val jsonMap = m.map.toList.map {
        case (id, state) =>
          (
            Id.idKeyEncoder(id),
            Json.obj(
              "revision" -> Guid.encodeGuid(state.revision),
              "data" -> encoder(state.data)
            )
          )
      }
      Json.obj(jsonMap: _*)
    }
  )

}

/**
  * The state of a piece of data in the Mirror
  * @param data           The data itself
  * @param revision       The current revision of the data
  * //@param incomingRefs   The Guids of all other pieces of data in the Mirror that contain references to this data
  * //@param outgoingRefs   The Guids of all other pieces of data in the Mirror that are referenced by this data
  * @tparam A             The data type
  */
private case class MirrorState[A](data: A, revision: Guid) {//}, incomingRefs: Set[Guid], outgoingRefs: Set[Guid]) {
//  def updatedIncomingRefs(id: Guid, add: Boolean): MirrorState[A] = copy(
//    incomingRefs = if (add) {
//      incomingRefs + id
//    } else {
//      incomingRefs - id
//    }
//  )
}

/**
  * A Mirror is just a map from Ids to MirrorState, which is the current value
  * for that Id and the revision of that value
  * @param map    Map from each known Guid to the data identified by that Guid
  */
class Mirror[U, A, D <: Delta[U, A]](private val map: Map[Id[A], MirrorState[A]]) {

  private def updateMap(newMap: Map[Id[A], MirrorState[A]]): Mirror[U, A, D] = new Mirror(newMap)

  /**
    * Retrieve the data for a reference, if reference is valid and data is present in mirror
    * @param ref  The reference
    * @return     The data if present, or None otherwise
    */
  def apply(ref: Ref[A]): Option[A] = getState(ref.id).map(_.data)

  private def getState(id: Id[A]): Option[MirrorState[A]] = map.get(id)

  def revisionOf(id: Id[A]): Option[Guid] = getState(id).map(_.revision)

//  def incomingRefs(guid: Guid): Set[Guid] = getState(guid).map(_.incomingRefs).getOrElse(Set.empty[Guid])
//  def outgoingRefs(guid: Guid): Set[Guid] = getState(guid).map(_.outgoingRefs).getOrElse(Set.empty[Guid])

  def get(id: Id[A]): Option[A] = getState(id).map(_.data)
  def revisionOf(ref: Ref[A]): Option[Guid] = revisionOf(ref.id)

//  private def incomingRefsFor(guid: Guid): Set[Guid] = {
//    map.foldLeft(Set.empty[Guid]){
//      case (refs, entry) =>
//        // If the entry represents data having an outgoing ref to our guid, then
//        // add that data's guid to the incoming refs.
//        // Ignore the outgoing refs of our own data
//        if (entry._2.outgoingRefs.contains(guid) && entry._1 != guid) {
//          refs + entry._1
//          // No outgoing ref from the entry's data to our id, so leave refs unaltered
//        } else {
//          refs
//        }
//    }
//  }

//  private def updateIncomingRefs(guid: Guid, outgoingRefs: Set[Guid], add: Boolean): Mirror = {
//    val map2 = outgoingRefs.foldLeft(map){
//      case (m, outgoingRef) =>
//        m.get(outgoingRef).fold {
//          // If data reached by outgoing ref is not in mirror, nothing to update
//          m
//        }{
//          // If data IS in the mirror, we update its mirror state to add/remove ourselves as an incoming ref
//          (otherMirrorState: MirrorState[_]) =>
//            m.updated(
//              outgoingRef,
//              otherMirrorState.updatedIncomingRefs(guid, add)
//            )
//        }
//    }
//    updateMap(map2)
//  }

  /**
    * Update the mirror to contain a value. If a value with the same Id is already in the mirror, it is
    * replaced by the new value.
    * A new revision is generated for the new value.
    *
    * @param a            The value
    * @param identifiable Provides Id for the value
    * @return             New Mirror with updated value
    */
  def updated(a: A)(implicit identifiable: Identifiable[A]): DeltaIO[U, Mirror[U, A, D]] = for {
    revision <- getGuid
  } yield updated(a, revision)

  /**
    * Update the mirror to contain a value. If a value with the same Id is already in the mirror, it is
    * replaced by the new value.
    * A new revision is generated for the new value.
    *
    * @param id           The id of the value
    * @param a            The value
    * @return             New Mirror with updated value
    */
  def updated(id: Id[A], a: A): DeltaIO[U, Mirror[U, A, D]] = for {
    revision <- getGuid
  } yield updated(id, a, revision)

  /**
    * Update the mirror to contain a value at a specified revision.
    * If a value with the same Id is already in the mirror, it is replaced by the new value.
    *
    * @param a            The value
    * @param revision     The revision of the value
    * @param identifiable Provides Id for the value
    * @return
    */
  def updated(a: A, revision: Guid)(implicit identifiable: Identifiable[A]): Mirror[U, A, D] = updated(identifiable.id(a), a, revision)

  /**
    * Update the mirror to contain a value at a specified revision.
    * If a value with the same Id is already in the mirror, it is replaced by the new value.
    *
    * @param id           The id of the value
    * @param a            The value
    * @param revision     The revision of the value
    * @return
    */
  def updated(id: Id[A], a: A, revision: Guid): Mirror[U, A, D] = {
    // Updated map for this data item, using the results of update, and new revision
    val map2 = map.updated(id, MirrorState(a, revision))

    // Updated mirror with the updated map
    new Mirror(map2)

    // TODO restore Incoming/outgoing refs version below
    //, plus updated incoming refs, and with the MirrorCodec added
    // in case we don't have it already
//    // Update refs in the updated data
//    val updateResult = mCodecA.deltaCodec.updateRefs(RefUpdateResult.noRefs(a), this)
//
//    // If data was already in mirror, incoming refs do not change just because data
//    // is updated. Otherwise build incoming refs from scratch
//    val incomingRefs = state.map(_.incomingRefs).getOrElse(incomingRefsFor(id))
//
//    // Updated map for this data item, using the results of update, and new revision
//    val map2 = map.updated(id, MirrorState(updateResult.data, revision, incomingRefs, updateResult.outgoingRefs, mCodecA))
//
//    // If data was already in mirror, look up previous outgoing refs - otherwise treat as empty
//    val previousOutgoingRefs = state.map(_.outgoingRefs).getOrElse(Set.empty)
//    val currentOutgoingRefs = updateResult.outgoingRefs
//
//    // Updated mirror with the updated map, plus updated incoming refs, and with the MirrorCodec added
//    // in case we don't have it already
//    val mirror2 = new Mirror(map2, types.updated(mCodecA.mirrorType, mCodecA))
//
//    // We look at our previous and current outgoing refs - where we have
//    // a new outgoing ref we need to add ourselves to the incoming refs of the
//    // newly-referenced data. Similarly where we have lost an outgoing ref we will
//    // remove ourselves from the incoming refs of the previously-referenced data.
//    val mirror3 = mirror2
//      .updateIncomingRefs(id, currentOutgoingRefs -- previousOutgoingRefs, add = true)
//      .updateIncomingRefs(id, previousOutgoingRefs -- currentOutgoingRefs, add = false)
//
//    // Finally, we need to update the refs in everything that points at us to get our new revision.
//    // This doesn't trigger any further updates, since the actual data in those
//    // data items that point at us hasn't changed.
//    updateDataForIds(incomingRefs, mirror3)
  }

  // TODO restore Incoming/outgoing refs version below
  //  /**
//    * Update a mirror by getting the state for each id in a set. If that id has a state,
//    * update references in the data, then update the mirror with that updated data.
//    * This is used (for example) when the revision of a piece of data in the mirror changes (or
//    * data is removed and so has no revision), in order to update all data that references
//    * the updated/removed data.
//    * @param ids    The set of ids whose data will have references updated
//    * @param mirror The initial mirror
//    * @return       A mirror with the required updates performed
//    */
//  private def updateDataForIds(ids: Set[Guid], mirror: Mirror): Mirror = {
//    // This doesn't trigger any further updates, since the actual data in those
//    // data items that point at us hasn't changed.
//    ids.foldLeft(mirror){
//      case (m, id) =>
//        m.getState(id).fold(
//          // If the data that refers to us is not in the mirror, no update
//          // This should not happen, since we only get an incoming reference when the data
//          // is added to the mirror, and when it is removed the incoming reference is cleared.
//          // However dangling incoming references do no harm other than triggering these
//          // no-ops.
//          m
//        )(
//          // If data is in mirror, update its references to get our new revision
//          state => {
//            val rur = state.mirrorCodec.deltaCodec.updateRefs(RefUpdateResult.noRefs(state.data), m)
//            m.updateMap(m.map.updated(id, state.copy(data = rur.data)))
//          }
//        )
//    }
//  }

  /**
    * Create a new Mirror with a data item not present (same mirror if data was
    * not in the mirror)
    * @param id Id of data to remove
    * @return   New mirror with data item not present
    */
  def removed(id: Id[A]): Mirror[U, A, D] = {
    getState(id).fold{
      this
    }{
      // First remove the data item from map
      // Then update the incoming refs of everything the data item referred to, to
      // remove the data item.
      _ => updateMap(map - id)

      // TODO restore Incoming/outgoing refs version below
//      state =>
//        // First remove the data item from map
//        // Then update the incoming refs of everything the data item referred to, to
//        // remove the data item.
//        updateMap(map - id.guid)
//
//        // First remove the data item from map
//        // Then update the incoming refs of everything the data item referred to, to
//        // remove the data item.
//        val mirror2 = updateMap(map - id).updateIncomingRefs(id, state.outgoingRefs, add = false)
//
//        // Finally, we need to update the refs in everything that points at the removed data so that they will have
//        // RefUnresolved, since data is no longer in the mirror.
//        // This doesn't trigger any further updates, since the actual data in those
//        // data items that point at the removed data hasn't changed.
//        updateDataForIds(state.incomingRefs, mirror2)
    }
  }

    override def toString: String = "Mirror(" + map + ")"
}
