package org.rebeam.tree.ref

import io.circe._
import org.rebeam.tree.Delta._
import org.rebeam.tree.DeltaCodecs.DeltaCodec
import org.rebeam.tree.Searchable
import org.rebeam.tree.sync._

import scala.collection.mutable.ArrayBuffer

/**
  * Represents a type of data referenced in a Mirror
  * @param name   The name of the type, must only be associated with one real type in a given mirror.
  */
case class MirrorType(name: String) extends AnyVal

/**
  * Typeclass covering everything needed to contain a type of data in a Mirror
  * @tparam A     The type of data
  */
trait MirrorCodec[A] {
  def mirrorType: MirrorType
  def encoder: Encoder[A]
  def decoder: Decoder[A]
  def deltaCodec: DeltaCodec[A]
  //  def searchable: Searchable[A, Guid]
}

object MirrorCodec {
  def apply[A](mirrorType: String)(implicit encoder: Encoder[A], decoder: Decoder[A], deltaCodec: DeltaCodec[A]): MirrorCodec[A] =
    MirrorCodecBasic(MirrorType(mirrorType), encoder, decoder, deltaCodec)
}

case class MirrorCodecBasic[A](mirrorType: MirrorType, encoder: Encoder[A], decoder: Decoder[A], deltaCodec: DeltaCodec[A]) extends MirrorCodec[A]

object Mirror {
  val empty = new Mirror(Map.empty, Map.empty)

  // There is no meaning to searching for refs in a mirror
  implicit val mirrorNotSearchableForGuid: Searchable[Mirror, Guid] = Searchable.notSearchable

  /**
    * An entry in the Mirror.
    * Note we use type Any - we ensure that data type, guid and codec are for the same type
    * at runtime by construction of map.
    * @param guid       Id of the data
    * @param codec      MirrorCodec so we can encode/decode mirror, etc.
    * @param revision   The current revision of the data
    * @param data       The data itself
    */
  private case class MirrorEntry(guid: Guid, codec: MirrorCodec[Any], revision: Guid, data: Any)

  /**
    * Decoder for mirrors containing only the types handled by the provided MirrorCodecs
    * @param codecs The MirrorCodecs to use to decode Mirror contents
    * @return       A Decoder[Mirror] handling provided content types
    */
  def decoder(codecs: MirrorCodec[_]*): Decoder[Mirror] = {
    val codecMap = codecs.toList.map(codec => (codec.mirrorType, codec)).toMap[MirrorType, MirrorCodec[_]]
    Decoder.instance[Mirror](
      c => c.keys match {

        case None => Left[DecodingFailure, Mirror](DecodingFailure("No fields in mirror Json", c.history))

        case Some(keys) =>
          val it = keys.iterator
          val entries = ArrayBuffer.empty[MirrorEntry]
          var failed: DecodingFailure = null

          while (failed.eq(null) && it.hasNext) {
            val field = it.next
            val atH = c.downField(field)

            Guid.guidKeyDecoder(field) match {

              case Some(guid) =>
                atH.keys.map(_.toSet) match {
                  case Some(set) if set.size == 1 =>
                    val mirrorType = MirrorType(set.head)
                    val mirrorCodec = codecMap.get(mirrorType)
                    mirrorCodec match {
                      case Some(codec) =>
                        val revAndDataC = atH.downField(set.head)

                        Guid.decodeGuid.tryDecode(revAndDataC.downField("revision")) match {
                          case Right(revision) =>
                            codec.decoder.tryDecode(revAndDataC.downField("data")) match {
                              case Right(data) =>
                                entries += MirrorEntry(guid, codec.asInstanceOf[MirrorCodec[Any]], revision, data)
                              case Left(error) => failed = error
                            }
                          case Left(error) => failed = error
                        }
                      case _ => failed = DecodingFailure("No MirrorCodec for type " + mirrorType, atH.history)
                    }
                  case _ => failed = DecodingFailure("Mirror should have entries with single field named after data type", atH.history)
                }
              case _ => failed = DecodingFailure("Invalid key in mirror", atH.history)
            }
          }

          if (failed.eq(null)) {
            Right(entries.foldLeft(Mirror.empty) {
              case (mirror, entry) => mirror.updated(Id[Any](entry.guid), entry.data, entry.revision)(entry.codec)
            })
          } else {
            Left(failed)
          }
      }
    )
  }


  /**
    * Encode as an object with fields from guids.
    * Each field contains an object named after the MirrorType of the data item,
    * containing its revision and data.
    */
  val encoder: Encoder[Mirror] = Encoder.instance[Mirror](
    m => {
      val jsonMap = m.map.toList.map {
        case (guid, state) =>
          val mc = state.mirrorCodec.asInstanceOf[MirrorCodec[Any]]
          (
            Guid.guidKeyEncoder(guid),
            Json.obj(
              mc.mirrorType.name -> Json.obj(
                "revision" -> Guid.encodeGuid(state.revision),
                "data" -> mc.encoder(state.data)
              )
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
  * @param mirrorCodec    A MirrorCodec for the data type
  * //@param incomingRefs   The Guids of all other pieces of data in the Mirror that contain references to this data
  * //@param outgoingRefs   The Guids of all other pieces of data in the Mirror that are referenced by this data
  * @tparam A             The data type
  */
private case class MirrorState[A](data: A, revision: Guid, mirrorCodec: MirrorCodec[A]) {//}, incomingRefs: Set[Guid], outgoingRefs: Set[Guid]) {
//  def updatedIncomingRefs(id: Guid, add: Boolean): MirrorState[A] = copy(
//    incomingRefs = if (add) {
//      incomingRefs + id
//    } else {
//      incomingRefs - id
//    }
//  )
}

/**
  * A Mirror handles the client side of synchronising a key value map between a client and server.
  * @param map    Map from each known Guid to the data identified by that Guid
  * @param types  Map from types of data in the mirror to the associated codecs
  */
class Mirror(private val map: Map[Guid, MirrorState[_]], private val types: Map[MirrorType, MirrorCodec[_]]) {

  private def updateMap(newMap: Map[Guid, MirrorState[_]]): Mirror = new Mirror(newMap, types)

  def updateType[A](mirrorType: MirrorType, mirrorCodec: MirrorCodec[A]) = new Mirror(map, types.updated(mirrorType, mirrorCodec))

  /**
    * Retrieve the data for a reference, if reference is valid and data is present in mirror
    * @param ref  The reference
    * @return     The data if present, or None otherwise
    */
  def apply[A](ref: Ref[A]): Option[A] = getState(ref.id).map(_.data)


  private def getState(guid: Guid): Option[MirrorState[_]] = map.get(guid)

  def revisionOf(guid: Guid): Option[Guid] = getState(guid).map(_.revision)

//  def incomingRefs(guid: Guid): Set[Guid] = getState(guid).map(_.incomingRefs).getOrElse(Set.empty[Guid])
//  def outgoingRefs(guid: Guid): Set[Guid] = getState(guid).map(_.outgoingRefs).getOrElse(Set.empty[Guid])


  private def getState[A](id: Id[A]): Option[MirrorState[A]] = map.get(id.guid).map(_.asInstanceOf[MirrorState[A]])

  def get[A](id: Id[A]): Option[A] = getState(id).map(_.data)
  def revisionOf[A](ref: Ref[A]): Option[Guid] = getState(ref.id).map(_.revision)

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
    * @param mCodecA      MirrorCodec for the value
    * @tparam A           Type of the value
    * @return             New Mirror with updated value
    */
  def updated[A](a: A)(implicit identifiable: Identifiable[A], mCodecA: MirrorCodec[A]): DeltaIO[Mirror] = for {
    revision <- getGuid
  } yield updated(a, revision)

  /**
    * Update the mirror to contain a value. If a value with the same Id is already in the mirror, it is
    * replaced by the new value.
    * A new revision is generated for the new value.
    *
    * @param id           The id of the value
    * @param a            The value
    * @param mCodecA      MirrorCodec for the value
    * @tparam A           Type of the value
    * @return             New Mirror with updated value
    */
  def updated[A](id: Id[A], a: A)(implicit mCodecA: MirrorCodec[A]): DeltaIO[Mirror] = for {
    revision <- getGuid
  } yield updated(id, a, revision)

  /**
    * Update the mirror to contain a value at a specified revision.
    * If a value with the same Id is already in the mirror, it is replaced by the new value.
    *
    * @param a            The value
    * @param revision     The revision of the value
    * @param identifiable Provides Id for the value
    * @param mCodecA      MirrorCodec for the value
    * @tparam A           Type of the value
    * @return
    */
  def updated[A](a: A, revision: Guid)(implicit identifiable: Identifiable[A], mCodecA: MirrorCodec[A]): Mirror = updated(identifiable.id(a), a, revision)

  /**
    * Update the mirror to contain a value at a specified revision.
    * If a value with the same Id is already in the mirror, it is replaced by the new value.
    *
    * @param id           The id of the value
    * @param a            The value
    * @param revision     The revision of the value
    * @param mCodecA      MirrorCodec for the value
    * @tparam A           Type of the value
    * @return
    */
  def updated[A](id: Id[A], a: A, revision: Guid)(implicit mCodecA: MirrorCodec[A]): Mirror = {
    // Updated map for this data item, using the results of update, and new revision
    val map2 = map.updated(id.guid, MirrorState(a, revision, mCodecA))

    // Updated mirror with the updated map, plus updated incoming refs, and with the MirrorCodec added
    // in case we don't have it already
    new Mirror(map2, types.updated(mCodecA.mirrorType, mCodecA))

    // TODO restore Incoming/outgoing refs version below
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
  def removed[A](id: Id[A]): Mirror = {
    getState(id).fold{
      this
    }{
      // First remove the data item from map
      // Then update the incoming refs of everything the data item referred to, to
      // remove the data item.
      _ => updateMap(map - id.guid)

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
