package org.rebeam.tree.ref

import io.circe._
import org.rebeam.tree.Delta
import org.rebeam.tree.Delta._
import org.rebeam.tree.DeltaCodecs.{DeltaCodec, RefUpdateResult}
import org.rebeam.tree.sync.Sync.{Guid, ToId}
import org.rebeam.tree.ref.Ref._

import scala.collection.mutable.ArrayBuffer

trait RefUpdater {
  /**
    * Update a ref to the latest version if necessary
    * @param ref  The ref to update
    * @tparam A   The type of the referent
    * @return     Some(updated ref) if update is needed, None otherwise
    */
  def updateRef[A](ref: Ref[A]): Option[Ref[A]]
}

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
  def deltaCodec: DeltaCodec[A]
  def decoder: Decoder[A]
  def encoder: Encoder[A]
  def mirrorType: MirrorType
}

case class MirrorCodecBasic[A](decoder: Decoder[A], encoder: Encoder[A], deltaCodec: DeltaCodec[A], mirrorType: MirrorType) extends MirrorCodec[A]

object MirrorCodec {
  def apply[A](mirrorType: String)(implicit decoder: Decoder[A], encoder: Encoder[A], deltaCodec: DeltaCodec[A]): MirrorCodec[A] =
    MirrorCodecBasic(decoder, encoder, deltaCodec, MirrorType(mirrorType))
}

object Mirror {
  val empty = new Mirror(Map.empty, Map.empty)

  implicit val guidKeyEncoder: KeyEncoder[Guid[_]] = new KeyEncoder[Guid[_]] {
    override def apply(key: Guid[_]): String = Guid.toString(key)
  }

  implicit val guidKeyDecoder = new KeyDecoder[Guid[_]] {
    override def apply(key: String): Option[Guid[_]] = Guid.fromString(key)
  }

  private case class MirrorEntry(guid: Guid[_], codec: MirrorCodec[Any], revision: Guid[_], data: Any)

  def decoder(codecs: MirrorCodec[_]*): Decoder[Mirror] = {
    val codecMap = codecs.toList.map(codec => (codec.mirrorType, codec)).toMap[MirrorType, MirrorCodec[_]]
    Decoder.instance[Mirror](
      c => c.fields match {

        case None => Left[DecodingFailure, Mirror](DecodingFailure("No fields in mirror Json", c.history))

        case Some(fields) =>
          val it = fields.iterator
          val entries = ArrayBuffer.empty[MirrorEntry]
          var failed: DecodingFailure = null

          while (failed.eq(null) && it.hasNext) {
            val field = it.next
            val atH = c.downField(field)

            guidKeyDecoder(field) match {

              case Some(guid) =>
                atH.fieldSet match {
                  case Some(set) if set.size == 1 =>
                    val mirrorType = MirrorType(set.head)
                    val mirrorCodec = codecMap.get(mirrorType)
                    mirrorCodec match {
                      case Some(codec) =>
                        val revAndDataC = atH.downField(set.head)

                        Guid.decodeGuid[Any].tryDecode(revAndDataC.downField("revision")) match {
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
              case (mirror, entry) => mirror.updated(entry.guid, entry.data, entry.revision)(entry.codec)
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
        case (guid, state) => {
          val mc = state.mirrorCodec.asInstanceOf[MirrorCodec[Any]]
          (
            guidKeyEncoder(guid),
            Json.obj(
              mc.mirrorType.name -> Json.obj(
                "revision" -> Guid.encodeGuid(state.revision),
                "data" -> mc.encoder(state.data)
              )
            )
          )
        }
      }
      Json.obj(jsonMap: _*)
    }
  )

}

/**
  * The state of a piece of data in the Mirror
  * @param data           The data itself
  * @param revision       The current revision of the data
  * @param incomingRefs   The Guids of all other pieces of data in the Mirror that contain references to this data
  * @param outgoingRefs   The Guids of all other pieces of data in the Mirror that are referenced by this data
  * @param mirrorCodec    A MirrorCodec for the data type
  * @tparam A             The data type
  */
private case class MirrorState[A](data: A, revision: Guid[A], incomingRefs: Set[Guid[_]], outgoingRefs: Set[Guid[_]], mirrorCodec: MirrorCodec[A]) {
  def updatedIncomingRefs(id: Guid[_], add: Boolean): MirrorState[A] = copy(
    incomingRefs = if (add) {
      incomingRefs + id
    } else {
      incomingRefs - id
    }
  )
}

/**
  * A Mirror handles the client side of synchronising a key value map between a client and server.
  * @param map    Map from each known Guid to the data identified by that Guid
  * @param types  Map from types of data in the mirror to the associated codecs
  */
class Mirror(private val map: Map[Guid[_], MirrorState[_]], private val types: Map[MirrorType, MirrorCodec[_]]) extends RefUpdater {

  private def updateMap(newMap: Map[Guid[_], MirrorState[_]]): Mirror = new Mirror(newMap, types)

  def updateType[A](mirrorType: MirrorType, mirrorCodec: MirrorCodec[A]) = new Mirror(map, types.updated(mirrorType, mirrorCodec))

  /**
    * Retrieve the data for a reference, if reference is valid and data is present in mirror
    * @param ref  The reference
    * @return     The data if present, or None otherwise
    */
  def apply[A](ref: Ref[A]): Option[A] = getState(ref.guid).map(_.data)
//    ref match {
//    case RefUnresolved(_) => None
//    case RefResolved(guid, revision) => getState(guid).filter(_.revision == revision).map(_.data)
//  }

  def revisionOf[A](ref: Ref[A]): Option[Guid[A]] = getState(ref.guid).map(_.revision)

  def revisionOf[A](guid: Guid[A]): Option[Guid[A]] = getState(guid).map(_.revision)

  def updateRef[A](ref: Ref[A]): Option[Ref[A]] = getState(ref.guid).fold[Option[Ref[A]]]{
    // If ref is not in mirror, update to unresolved
    Some(RefUnresolved(ref.guid))
  }{
    // If ref is in mirror, update to resolved at current revision
    state => Some(RefResolved(ref.guid, state.revision))
  // Skip update if new ref is equal to old one
  }.filterNot(_ == ref)

  def incomingRefs(id: Guid[_]): Set[Guid[_]] = getState(id).map(_.incomingRefs).getOrElse(Set.empty[Guid[_]])
  def outgoingRefs(id: Guid[_]): Set[Guid[_]] = getState(id).map(_.outgoingRefs).getOrElse(Set.empty[Guid[_]])

  def get[A](id: Guid[A]): Option[A] = getState(id).map(_.data)

  private def getState[A](id: Guid[A]): Option[MirrorState[A]] = map.get(id).map(_.asInstanceOf[MirrorState[A]])

  private def incomingRefsFor(id: Guid[_]): Set[Guid[_]] = {
    map.foldLeft(Set.empty[Guid[_]]){
      case (refs, entry) =>
        // If the entry represents data having an outgoing ref to our id, then
        // add that data's id to the incoming refs.
        // Ignore the outgoing refs of our own data
        if (entry._2.outgoingRefs.contains(id) && entry._1 != id) {
          refs + entry._1
          // No outgoing ref from the entry's data to our id, so leave refs unaltered
        } else {
          refs
        }
    }
  }

  private def updateIncomingRefs(id: Guid[_], outgoingRefs: Set[Guid[_]], add: Boolean): Mirror = {
    val map2 = outgoingRefs.foldLeft(map){
      case (m, outgoingRef) =>
        m.get(outgoingRef).fold {
          // If data reached by outgoing ref is not in mirror, nothing to update
          m
        }{
          // If data IS in the mirror, we update its mirror state to add/remove ourselves as an incoming ref
          (otherMirrorState: MirrorState[_]) =>
            m.updated(
              outgoingRef,
              otherMirrorState.updatedIncomingRefs(id, add)
            )
        }
    }
    updateMap(map2)
  }

  def updated[A](a: A)(implicit toId: ToId[A], mCodecA: MirrorCodec[A]): DeltaIO[Mirror] = for {
    revision <- getId[A]
  } yield updated(a, revision)

  def updated[A](id: Guid[A], a: A)(implicit mCodecA: MirrorCodec[A]): DeltaIO[Mirror] = for {
    revision <- getId[A]
  } yield updated(id, a, revision)

  def updated[A](a: A, revision: Guid[A])(implicit toId: ToId[A], mCodecA: MirrorCodec[A]): Mirror = updated(toId.id(a), a, revision)

  def updated[A](id: Guid[A], a: A, revision: Guid[A])(implicit mCodecA: MirrorCodec[A]): Mirror = {
    val state = getState(id)

    // Update refs in the updated data
    val updateResult = mCodecA.deltaCodec.updateRefs(RefUpdateResult.noRefs(a), this)

    // If data was already in mirror, incoming refs do not change just because data
    // is updated. Otherwise build incoming refs from scratch
    val incomingRefs = state.map(_.incomingRefs).getOrElse(incomingRefsFor(id))

    // Updated map for this data item, using the results of update, and new revision
    val map2 = map.updated(id, MirrorState(updateResult.data, revision, incomingRefs, updateResult.outgoingRefs, mCodecA))

    // If data was already in mirror, look up previous outgoing refs - otherwise treat as empty
    val previousOutgoingRefs = state.map(_.outgoingRefs).getOrElse(Set.empty)
    val currentOutgoingRefs = updateResult.outgoingRefs

    // Updated mirror with the updated map, plus updated incoming refs, and with the MirrorCodec added
    // in case we don't have it already
    val mirror2 = new Mirror(map2, types.updated(mCodecA.mirrorType, mCodecA))

    // We look at our previous and current outgoing refs - where we have
    // a new outgoing ref we need to add ourselves to the incoming refs of the
    // newly-referenced data. Similarly where we have lost an outgoing ref we will
    // remove ourselves from the incoming refs of the previously-referenced data.
    val mirror3 = mirror2
      .updateIncomingRefs(id, currentOutgoingRefs -- previousOutgoingRefs, add = true)
      .updateIncomingRefs(id, previousOutgoingRefs -- currentOutgoingRefs, add = false)

    // Finally, we need to update the refs in everything that points at us to get our new revision.
    // This doesn't trigger any further updates, since the actual data in those
    // data items that point at us hasn't changed.
    updateDataForIds(incomingRefs, mirror3)
  }

  /**
    * Update a mirror by getting the state for each id in a set. If that id has a state,
    * update references in the data, then update the mirror with that updated data.
    * This is used (for example) when the revision of a piece of data in the mirror changes (or
    * data is removed and so has no revision), in order to update all data that references
    * the updated/removed data.
    * @param ids    The set of ids whose data will have references updated
    * @param mirror The initial mirror
    * @return       A mirror with the required updates performed
    */
  private def updateDataForIds(ids: Set[Guid[_]], mirror: Mirror): Mirror = {
    // This doesn't trigger any further updates, since the actual data in those
    // data items that point at us hasn't changed.
    ids.foldLeft(mirror){
      case (m, id) =>
        m.getState(id).fold(
          // If the data that refers to us is not in the mirror, no update
          // This should not happen, since we only get an incoming reference when the data
          // is added to the mirror, and when it is removed the incoming reference is cleared.
          // However dangling incoming references do no harm other than triggering these
          // no-ops.
          m
        )(
          // If data is in mirror, update its references to get our new revision
          state => {
            val rur = state.mirrorCodec.deltaCodec.updateRefs(RefUpdateResult.noRefs(state.data), m)
            m.updateMap(m.map.updated(id, state.copy(data = rur.data)))
          }
        )
    }
  }

  /**
    * Create a new Mirror with a data item not present (same mirror if data was
    * not in the mirror)
    * @param id Id of data to remove
    * @return   New mirror with data item not present
    */
  def removed[A](id: Guid[A]): Mirror = {
    getState(id).fold{
      this
    }{
      state =>
        // First remove the data item from map
        // Then update the incoming refs of everything the data item referred to, to
        // remove the data item.
        val mirror2 = updateMap(map - id).updateIncomingRefs(id, state.outgoingRefs, add = false)

        // Finally, we need to update the refs in everything that points at the removed data so that they will have
        // RefUnresolved, since data is no longer in the mirror.
        // This doesn't trigger any further updates, since the actual data in those
        // data items that point at the removed data hasn't changed.
        updateDataForIds(state.incomingRefs, mirror2)
    }
  }

    override def toString: String = "Mirror(" + map + ")"
}
