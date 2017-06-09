package org.rebeam.tree.ref

import org.rebeam.tree.DeltaCodecs.{DeltaCodec, RefUpdateResult}
import org.rebeam.tree.sync.Sync.Ref.{RefResolved, RefUnresolved}
import org.rebeam.tree.sync.Sync.{Guid, Ref, ToId}

trait DeltaCache {
    /**
      * Retrieve the data for a reference, if reference is valid and data is present in cache
      * @param ref  The reference
      * @tparam A   The type of data
      * @return     The data if present, or None
      */
    def apply[A](ref: Ref[A]): Option[A]

    /**
      * Update a ref to the latest version if necessary
      * @param ref  The ref to update
      * @tparam A   The type of the referent
      * @return     Some(updated ref) if update is needed, None otherwise
      */
    def updateRef[A](ref: Ref[A]): Option[Ref[A]]
}

private case class CacheState[A](data: A, revision: Long, incomingRefs: Set[Guid[_]], outgoingRefs: Set[Guid[_]]) {
  def updatedIncomingRefs(id: Guid[_], add: Boolean): CacheState[A] = copy(
    incomingRefs = if (add) {
      incomingRefs + id
    } else {
      incomingRefs - id
    }
  )
}

class Cache[M](private val map: Map[Guid[_], CacheState[_]])(implicit dCodecM: DeltaCodec[M]) extends DeltaCache {

  def apply[A](ref: Ref[A]): Option[A] = ref match {
    case RefUnresolved(_) => None
    case RefResolved(guid, revision) => getState(guid).filter(_.revision == revision).map(_.data)
  }

  def updateRef[A](ref: Ref[A]): Option[Ref[A]] = getState(ref.guid).fold[Option[Ref[A]]]{
    // If ref is not in cache, update to unresolved
    Some(RefUnresolved(ref.guid))
  }{
    // If ref is in cache, update to resolved at current revision
    state => Some(RefResolved(ref.guid, state.revision))
  // Skip update if new ref is equal to old one
  }.filterNot(_ == ref)

  def incomingRefs[A](id: Guid[A]): Set[Guid[_]] = getState(id).map(_.incomingRefs).getOrElse(Set.empty[Guid[_]])
  def outgoingRefs[A](id: Guid[A]): Set[Guid[_]] = getState(id).map(_.outgoingRefs).getOrElse(Set.empty[Guid[_]])

  def get[A](id: Guid[A]): Option[A] = getState(id).map(_.data)

  private def getState[A](id: Guid[A]): Option[CacheState[A]] = map.get(id).map(_.asInstanceOf[CacheState[A]])

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

  private def updateIncomingRefs(id: Guid[_], outgoingRefs: Set[Guid[_]], add: Boolean): Cache[M] = {
    val map2 = outgoingRefs.foldLeft(map){
      case (m, outgoingRef) =>
        m.get(outgoingRef).fold {
          // If data reached by outgoing ref is not in cache, nothing to update
          m
        }{
          // If data IS in the cache, we update its cache state to add/remove ourselves as an incoming ref
          (otherCacheState: CacheState[_]) =>
            m.updated(
              outgoingRef,
              otherCacheState.updatedIncomingRefs(id, add)
            )
        }
    }
    new Cache[M](map2)
  }

  def updated[A <: M](a: A)(implicit toId: ToId[A]): Cache[M] = updated(toId.id(a), a)

  def updated[A <: M](id: Guid[A], a: A): Cache[M] = {
    val state = getState(id)

    // Next revision, or start from 0
    val updatedRev = state.map(_.revision + 1).getOrElse(0L)

    // Update refs in the updated data
    val updateResult = dCodecM.updateRefs(RefUpdateResult.noRefs(a), this)

    // If data was already in cache, incoming refs do not change just because data
    // is updated. Otherwise build incoming refs from scratch
    val incomingRefs = state.map(_.incomingRefs).getOrElse(incomingRefsFor(id))

    // Updated map for this data item, using the results of update, and updated rev
    val map2 = map.updated(id, CacheState(updateResult.data, updatedRev, incomingRefs, updateResult.outgoingRefs))

    // If data was already in cache, look up previous outgoing refs - otherwise treat as empty
    val previousOutgoingRefs = state.map(_.outgoingRefs).getOrElse(Set.empty)
    val currentOutgoingRefs = updateResult.outgoingRefs

    // Updated cache with the updated map, plus updated incoming refs.
    val cache2 = new Cache[M](map2)

    // We look at our previous and current outgoing refs - where we have
    // a new outgoing ref we need to add ourselves to the incoming refs of the
    // newly-referenced data. Similarly where we have lost an outgoing ref we will
    // remove ourselves from the incoming refs of the previously-referenced data.
    val cache3 = cache2
      .updateIncomingRefs(id, currentOutgoingRefs -- previousOutgoingRefs, add = true)
      .updateIncomingRefs(id, previousOutgoingRefs -- currentOutgoingRefs, add = false)

    // Finally, we need to update the refs in everything that points at us to get our new revision.
    // This doesn't trigger any further updates, since the actual data in those
    // data items that point at us hasn't changed.
    incomingRefs.foldLeft(cache3){
      case (c, incomingId) =>
        c.getState(incomingId).fold(
          // If the data that refers to us is not in the cache, no update
          // This should not happen, since we only get an incoming reference when the data
          // is added to the cache, and when it is removed the incoming reference is cleared.
          // However dangling incoming references do no harm other than triggering these
          // no-ops.
          c
        )(
          // If data is in cache, update its references to get our new revision
          state => {
            val rur = dCodecM.updateRefs(RefUpdateResult.noRefs(state.data.asInstanceOf[M]), c)
            new Cache[M](c.map.updated(incomingId, state.copy(data = rur.data)))
          }
        )
    }
  }

  /**
    * Create a new Cache with a data item not present (same cache if data was
    * not in the cache)
    * @param id Id of data to remove
    * @tparam A Type of data to remove
    * @return   New cache with data item not present
    */
  def removed[A <: M](id: Guid[A]): Cache[M] = {
    getState(id).fold{
      this
    }{
      state =>
        // First remove the data item from map
        // Then update the incoming refs of everything the data item referred to, to
        // remove the data item.
        val cache2 = new Cache[M](map - id).updateIncomingRefs(id, state.outgoingRefs, add = false)

        // Finally, we need to update the refs in everything that points at the removed data so that they will have
        // RefUnresolved, since data is no longer in the cache.
        // This doesn't trigger any further updates, since the actual data in those
        // data items that point at the removed data hasn't changed.
        state.incomingRefs.foldLeft(cache2){
          case (c, incomingId) =>
            c.getState(incomingId).fold(
              // If the data that refers to removed data is not in the cache, no update
              // This should not happen, since we only get an incoming reference when the data
              // is added to the cache, and when it is removed the incoming reference is cleared.
              // However dangling incoming references do no harm other than triggering these
              // no-ops.
              c
            )(
              // If data is in cache, update its references to get an unresolved ref for the removed data
              state => {
                val rur = dCodecM.updateRefs(RefUpdateResult.noRefs(state.data.asInstanceOf[M]), c)
                new Cache[M](c.map.updated(incomingId, state.copy(data = rur.data)))
              }
            )
        }

    }
  }

    override def toString: String = "Cache(" + map + ")"
}

object Cache {
  def empty[M: DeltaCodec] = new Cache[M](Map.empty)
}