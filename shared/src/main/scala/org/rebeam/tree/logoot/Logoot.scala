package org.rebeam.tree.logoot

import org.rebeam.tree.Delta.{DeltaIO, _}
import org.rebeam.tree.sync.Sync.{ClientId, Guid, HasId}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

/**
  * Implementation of the Logoot CRDT based on https://hal.archives-ouvertes.fr/inria-00432368/document
  *
  * Acts as a List[A], but using an id per element of the list that has a total ordering, allowing
  * inserts, deletes and edits of elements from different clients to be merged neatly.
  * One approach to commutative editing of list elements is to give the elements their own Guids, and
  * use these to find them in the list for editing (or to put the elements in a Mirror and use a list of Refs),
  * thereby avoiding the use of plain integer indices into the list, which do not commute when we reorder, insert
  * or delete elements.
  * Using Guids works for editing and deleting elements, but does not give a commutative system for reordering or
  * inserting elements, and introduces a Guid that might otherwise be unnecessary. Logoot essentially gives
  * each element an associated id (including a Guid) based on its order in the list relative to other elements, which
  * is suitable for all operations. Note that this id is globally unique because it contains a Guid, but is only
  * valid within the particular Logoot list by which it is generated.
  *
  * Note that this implementation does NOT use the algorithms for generating new positions given in
  * the original paper, since I suspect they are flawed:
  *
  *  1. There is almost certainly an off by one error in generateLinePositions, which seems to have
  *     been corrected in the logoot-undo paper. This is obviously easily fixable but worrying.
  *  2. The generateLinePositions algorithm assumes that we can always generate a new position using just
  *     the "pos" integers of the positions p and q. However it seems to be possible for site identifiers
  *     to affect the ordering of position identifiers such that their pos integers are NOT in order, and
  *     this will then cause generateLinePositions to iterate forever.
  *     E.g. assume we are operating on two sites, using base 10 - if we have two positions that look like
  *     [2s, 6s] then [2t, 3t], where in each identifier the numbers are the pos values, and the letters represent
  *     the site id, and where s < t, we will order the positions as shown since the identifier 2s is less than
  *     2t. However we then just use the numbers "26" and "23" as the start and end "numbers" in base 10 of
  *     the search. With prefix size 1 we get 2 and 2, so no interval, with prefix size 2 we get 26 and 23, so
  *     "-3" interval, and then for prefix size 3 we get 260 and 230, "-30" interval, and so on forever,
  *     never finding a gap.
  *     Can we actually end up with a list containing [2s, 6s], [2t, 3t] as adjacent positions? I think we can,
  *     since we can start from an empty list at sites s and t, and on each site (in parallel without communications)
  *     generate 9 new positions - this fully occupies the first identifier, so we get
  *     [[1s], [2s] ... [9s]] on site s, and [[1t], [2t] ... [9t]] on site t.
  *     Then we insert a new line between [2s] and [3s] on site s, giving us a random choice for second identifier -
  *     say we choose [2s, 6s]. On site t we similarly insert between [2t] and [3t], but here we choose [2t, 3t].
  *     Now site s receives the newly generates lines from site t, and inserts them into its own list, ending up
  *     with [[1s], [1t], [2s], [2s, 6s], [2t], [2t, 3t], [3s], [3t] ...]. We then delete all but the positions
  *     having two identifiers, and get our example list where generateLinePositions fails.
  *
  * Therefore we use the simpler algorithm from https://github.com/usecanvas/logoot-js/blob/master/lib/logoot/sequence.js
  * This works on a strictly identifier-by-identifier basis, not assuming that the ordering using site indices is
  * consistent with the ordering using pos only. We just inspect each pair of identifiers and insert a single new identifier
  * as soon as we can.
  *
  */
object Logoot {

  /**
    * The minimum value of pos field of an Identifier. We use pos values from this value to maxValue, inclusive
    */
  val minPosValue: Int = 0

  /**
    * The maximum value of pos field of an Identifier. We use pos values from minValue to this value, inclusive
    */
  val maxPosValue: Int = Int.MaxValue

  /**
    * A default lower ident for defining firstPosition, and for use as a default lower
    * limit when choosing new identifiers. Not actually the minimum identifier - we
    * could have minPosValue with a negative client id, although we don't generate this.
    * ClientId value is not important for the valid operation of the Logoot. However 0
    * is conventionally used for initial data, and >= 1 for actual clients.
    */
  val lowerIdent: Identifier = Identifier(minPosValue, ClientId(0))

  /**
    * A default lower ident for defining lastPosition, and for use as a default upper
    * limit when choosing new identifiers. Not actually the maximum identifier - we
    * can have maxPosValue with a negative client id.
    * ClientId value is not important for the valid operation of the Logoot. However 0
    * is conventionally used for initial data, and >= 1 for actual clients.
    */
  val upperIdent: Identifier = Identifier(maxPosValue, ClientId(0))

  /**
    * The first/minimum position. Logically considered to be present in sequence when
    * inserting before the first element.
    */
  val firstPosition = Position(lowerIdent)

  /**
    * The last/maximum position. Logically considered to be present in sequence when
    * inserting after the first element.
    */
  val lastPosition = Position(upperIdent)

  /**
    * An identifier
    * @param pos      An integer from minPosValue to maxPosValue inclusive
    * @param clientId The client that generated the Identifier.
    *                 It is assumed that each client has a single copy
    *                 of each Document, or at least does not edit multiple copies asynchronously.
    *                 This is reasonable since all editing with a client is synchronous.
    *                 Multiple clients could be used for asynchronous editing if required.
    */
  case class Identifier(pos: Int, clientId: ClientId)

  /**
    * Compare Identifiers by pos, and then by site (using ClientId's id value)
    */
  implicit val identifierOrdering: Ordering[Identifier] = new Ordering[Identifier] {
    def compare(a: Identifier, b: Identifier): Int = {
      val byPos = a.pos.compareTo(b.pos)
      if (byPos == 0) {
        a.clientId.id.compareTo(b.clientId.id)
      } else {
        byPos
      }
    }
  }

  // A Position is just a list of identifiers
  type Position = List[Identifier]

  object Position {
    def apply(xs: Identifier*): Position = List.apply(xs:_*)
    def empty: Position = Nil
  }

  /**
    * Compare Positions by their identifiers, in order, for the indices
    * present in both Positions. If all are equal, compare by number of
    * identifiers.
    */
  implicit val positionOrdering: Ordering[Position] = new Ordering[Position] {
    override def compare(x: Position, y: Position): Int = {

      val xe = x.iterator
      val ye = y.iterator

      while (xe.hasNext && ye.hasNext) {
        val res = identifierOrdering.compare(xe.next(), ye.next())
        if (res != 0) return res
      }

      // x is < y if it has less elements (therefore xe.hasNext is false and ye.hasNext is true),
      // or equal if they have same number. Note false < true.
      xe.hasNext.compareTo(ye.hasNext)
    }
  }

  /**
    * A position identifier, having a list of positions and a guid.
    * The Guid makes this PositionIdentifier globally unique, so it can
    * be used as an identifier for lines to delete or edit.
    * The original logoot paper uses the combination of site id and
    * a local clock value to do this - the guid is slightly excessive since
    * we only really need its clientDeltaId and id  - we already have the clientId
    * from the Position itself, but for consistency we use the whole thing.
    * @param position   The position
    * @param id         Guid
    */
  case class PositionId(position: Position, id: Guid[PositionId]) extends HasId[PositionId]

  /**
    * Compare PositionIdentifiers by their position only.
    */
  implicit val positionIdOrdering: Ordering[PositionId] = new Ordering[PositionId] {
    override def compare(x: PositionId, y: PositionId): Int = positionOrdering.compare(x.position, y.position)
  }

  /**
    * A sequence - just a sorted map from PositionId to entries
    * Beginning (first) and end (last) identifier are implicit, and are Logoot.beginningIdentifier and Logoot.endIdentifier
    *
    * FIXME make this a private class with a trait, to prevent construction with arbitrary entries?
    * Construction should be starting from empty, or providing a list of As
    *
    * TODO An interesting approach to growth of positions would be to periodically regenerate them - in most cases
    * this could produce 1-length positions. We would need to track which "regeneration" of the position was being
    * used for insertion, and either:
    *   1. just reject old ones (confusing old clients), while presumably trying to avoid this by only regenerating
    *      when there are no clients connected, or
    *   2. retain a mapping from the previous generation's positions to the current, for long enough that all clients
    *      should have been informed of the new generation, or can be ignored because their lag is too great.
    *      So we would use a Delta to regenerate, and send this action to all clients. Until a timeout elapses, we
    *      would have both generations stored in the sequence, and old generation positions would be transparently
    *      updated to their new equivalents before inserting, and then at the timeout we would run another delta to
    *      remove the old generation. Any old-generation positions received after this would be rejected. Because the
    *      regeneration and dropping of old generation would be performed as deltas, they would be consistent when
    *      applied back on all clients. The only downside would be that dropping a generation would "kill" any deltas
    *      from clients that were generated with the old generation. However we _don't_ guarantee that all operations
    *      are commutative, we just require that they will either commute or do nothing when they cannot commute.
    *
    * @param map  The sorted map from position ids to entries
    * @tparam A       The type of value in entries
    */
  case class Sequence[A](map: SortedMap[PositionId, A]) {

    lazy val values: List[A] = map.values.toList
    lazy val pids: List[PositionId] = map.keys.toList

    /**
      * Create a position allowing insertion at a given index in the
      * sequence. The position will be just before the value at the specified
      * index, so the newly inserted value will be at that index in the
      * updated list.
      * Index is constrained to range 0 to size of sequence, where size
      * indicates appending to the end of the sequence.
      * @param index  The insertion index
      * @return       A DeltaIO producing a suitable insertion position
      */
    def insertionPosition(index: Int): DeltaIO[Position] = {
      // Constrain index to 0 to size of sequence (i.e. appending to end).
      val i = Math.min(Math.max(index, 0), pids.size)

      // Find position before and after insertion, handling cases before
      // and after end of actual entries - we use canonical beginning and
      // end positions
      val (p, q) =
        if (pids.isEmpty) {
          (firstPosition, lastPosition)
        } else if (i == 0) {
          (firstPosition, pids.head.position)
        } else if (i == pids.size) {
          (pids.last.position, lastPosition)
        } else {
          (pids(i - 1).position, pids(i).position)
        }

      positionBetween(p, q)
    }


    /**
      * Create a Sequence with a new entry inserted. Any existing
      * entries with the same pid.id will be deleted, even if they
      * are at different positions, so this can
      * be used to move an id to a new line at the same time as
      * updating the value
      * @param pid  The position id for the new entry
      * @param a    The value of the entry
      * @return     A sequence with the new entry inserted
      */
    def inserted(pid: PositionId, a: A): Sequence[A] =
      deleted(pid.id).copy(map.updated(pid, a))

    /**
      * Create a Sequence with an entry deleted. If there
      * is no entry with a positionId matching the guid then
      * result is the same sequence.
      * @param guid The guid of the PositionId for the entry
      * @return     A sequence with any matching entry removed
      */
    def deleted(guid: Guid[PositionId]): Sequence[A] =
      copy(map -- map.keySet.filter(_.id == guid))

    /**
      * Create a sequence with an entry moved. If there is
      * are any entries on lines with Guid matching the supplied
      * pid, then those entries are deleted, and a new entry
      * is inserted with the supplied pid and the value from
      * one of the deleted entries.
      * @param pid
      * @return
      */
    def moved(pid: PositionId): Sequence[A] = {
      // Find any entries with id matching the pid we are moving to
      val oldEntries = map.filter(e => e._1.id == pid.id)
      oldEntries.headOption match {
        // There are no old entries, so nothing to move, sequence unaltered
        case None => this

        // There is at least one old entry. Delete all old entries,
        // and move the first old entry's value to the new pid
        case Some((_, a)) =>
          val oldEntriesDeleted = map -- oldEntries.keys
          copy(oldEntriesDeleted.updated(pid, a))
      }
    }
  }

  object Sequence {
    /**
      * An empty sequence
      * @tparam A Type of value in sequence
      * @return   Empty sequence
      */
    def empty[A]: Sequence[A] = Sequence[A](SortedMap.empty)
  }

  private[logoot] def minIdent(x: Identifier, y: Identifier): Identifier =
    if (identifierOrdering.compare(x, y) < 0) x else y

  /**
    * Invariants:
    *
    *   1. All Identifiers have a pos >= minPosValue and <= maxPosValue.
    *   2. All Positions are in one of three classes:
    *     F. Identical to firstPosition, (minPosValue, 0)
    *     L. Identical to lastPosition, (maxPosValue, 0)
    *     G. Generated by this function
    *
    *   3. Only positions of class G, which we will call generated positions, are ever present in Sequences directly.
    *   There is one position of class F which is implicitly at the start of all sequences, and used for inserting
    *   new positions before all generated positions. Similarly there is one of class L implicitly at the
    *   end of each sequence, for inserting after all generated positions.
    *   4. All generated positions have a final identifier with a clientId from the client that generated them.
    *   5. All generated positions have a final identifier with a pos strictly > minPosValue. (Note that L
    *      also has this property, so all positions that can be "q" in requirement 2 have this property)
    *
    * Requirements:
    *
    *   1. All Positions in a sequence form an ordered sequence with no duplicates.
    *   2. We can produce a Position r between any pair of Positions p and q so that p < r < q
    *
    * Interface:
    *
    *   1. Sequences are only changed by inserting and deleting entries.
    *   2. Entries are inserted by generating a new Position between a pair of adjacent existing positions,
    *      i.e. two positions p and q where p < q and there is no position r in the sequence such that p < r < q
    *   3. Entries are deleted by simply removing them from the Sequence
    *
    * Ordering
    *
    *   1. Identifiers are a pos and a clientId, and are compared by pos then (if pos is equal) by clientId
    *   2. Positions are a list of Identifiers, and are compared identifier by identifier from first to last. Say
    *      we are comparing Positions p and q - if for all indices where both p and q have an Identifier, they have
    *      the same Identifier, then the longer position is considered to be greater. If Positions have the same
    *      Identifiers and same length then they are the same.
    *
    * Notes
    *
    *   1. PositionIdentifiers add a Guid to a Position. The PositionIdentifier is used entirely for ensuring
    *      uniqueness and looking up PositionIdentifiers directly. They are irrelevant for Positions themselves.
    *
    *
    * Based on the above, the only challenging task is to produce a function (this one) that can produce a generated
    * position meeting the invariants and requirements to insert a new entry.
    *
    * This can be expressed recursively. At each recursion stage i we term the previous recursion's result l
    * (empty on first recursion), and choose a new ident ri (generated at the ith recursion, and also becomes the
    * ith ident in the generated position) to form the result of this recursion, r = l + ri. On the final recursion
    * this yields the final position f.
    *
    * We will now consider all cases for p and q, to show that in each case the algorithm will produce a result f
    * s.t. p < f < q.
    *
    * ## p shorter than q.
    *
    * p < q, so pi <= qi for all i. Note p may be a just a prefix of q.
    *
    * While i is inside p, we may pick ri = pi for all digits where there is no ri s.t. pi < ri < qi,
    * otherwise we pick ri.pos randomly from the values that meet that condition, to produce f and terminate.
    *
    * We know that pi is a valid choice since a prefix of p followed by ri s.t. pi < ri < qi will yield p < f < q.
    *
    * We additionally know that if at any i we observe pi < qi, but there is no ri s.t. pi < ri < qi, we may from
    * the next recursion on substitute upperIdent for qi. This is because we will have an l s.t. l + ri < q
    * for all subsequent recursions - there is no upper limit on idents to comply with r < q, and upperIdent is
    * sufficient to give the upper limit part of invariant 1.
    *
    * If we do not terminate while inside p, we will reach the end of p with l = p. On subsequent iterations we know
    * that any r will be p ++ n, for some non-empty n, and so is greater than p. Thus we have no remaining requirement
    * on minimum ri, and use min(lowerIdent, qi). This will obviously always yield r < q as long as l < q. We will then
    * definitely terminate since pIdent is <= qi so we can proceed until we reach the final ident of qi which is
    * guaranteed to have pos > 0, allowing use of pos 0 to produce a strictly lower r - this will either be f or the
    * next ident will produce f since it can lie anywhere in lowerIdent < ri < upperIdent.
    *
    * ## p and q equal length
    *
    * Only the differences from "p shorter than q" will be described.
    *
    * p < q, so there is a first i where pi < qi. On this i we will either have some ri s.t. pi < ri < qi
    * and we terminate, or we will choose ri = pi so that upper limit of ri is unconstrained for later recursions.
    * As above, we now know we can keep adding ri until we reach the final digit of qi where we can either terminate
    * or append ri with pos = minValue and then terminate on the next recursion.
    *
    * ## p longer than q
    *
    * Only the differences from "p and q equal length" will be described.
    *
    * p < q, so again there is a first i where pi < qi, and at this point upper limit qIdent becomes unconstrained.
    * From this point while we are within p, pIdent will be pi, and qIdent will be upperIdent. If we don't terminate
    * (i.e. if pi is too close to or even greater than upperIdent) we will choose p, retaining the option of being > p
    * in future, until eventually we either terminate with pi, or terminate on the first recursion after p is exhausted,
    * where we can choose any ident meeting invariants.
    *
    * //TODO can/should we use the clientId parameter instead of default clientId(0) in pIdent and qIdent?
    *

    * @param p          We will generate a position > p
    * @param q          We will generate a position < q
    * @param r          The prefix of result already generated - we will add identifier(s) to this to produce result.
    * @param clientId   The clientId to be used for the final identifier in the result
    * @return           A Position > p and < q and meeting invariants
    */
  @tailrec
  private[logoot] def positionBetweenRec(p: Position, q: Position, r: Position, clientId: ClientId): Position = {

    // We need a permissive tail that is empty if list is empty, so we can
    // trim down p and q as we recurse
    def rest(pos: Position): Position = if (pos.isEmpty) Nil else pos.tail

    // Use the head of p and q, if they are empty then pad with firstPosition or lastPosition respectively,
    // except that we use qIdent instead of the firstPosition iff it is lower. This can happen if p has
    // an entry strictly lower than one in q, and is shorter than q.
    // These are used as the limits on the range of the new ident added to r to produce a result or to
    // recurse.
    val qIdent = q.headOption.getOrElse(upperIdent)
    val pIdent = p.headOption.getOrElse(minIdent(lowerIdent, qIdent))

    //println(s"r now $r, pIdent $pIdent, qIdent $qIdent")

    identifierOrdering.compare(pIdent, qIdent) match {
      // pIdent < qIdent - we can try to insert at this digit
      case -1 =>
        val posDiff = qIdent.pos - pIdent.pos

        //println(s"pIdent < qIdent, posDiff $posDiff")

        // We have a gap in pos, so choose a random pos in that gap.
        // We lie between p and q based on pos only, so can use our own site
        if (posDiff > 1) {
          //println(s"inserting into $posDiff gap")

          val random = 1 // TODO random value from 1 to diff - 1, inclusive
          // A: Gap in pos, so make sure new pos is at least old pos + 1
          // This ensures pos > minPosValue since we know pIdent.pos is a valid
          // pos and so must be >= minPosValue.
          r :+ Identifier(pIdent.pos + random, clientId)

        // No gap, try based on client id differences
        } else {

          // Note that the cases below except "else" allow for reusing a pos value
          // when site is suitable to order the resulting identifiers correctly.
          // However this isn't necessary for the algorithm to operate, and may
          // well be a corner case that is not worthwhile - it will tend to reduce
          // growth rate of positions while increasing complexity/time of algorithm.

          val pHasLowerClient = clientId.id > pIdent.clientId.id
          val qHasHigherClient = clientId.id < qIdent.clientId.id
          val pInvariant5 = pIdent.pos > minPosValue && pIdent.pos < maxPosValue

          //println(s"no gap, comparing clientIds, we have ${clientId.id}, greater than p's (${pIdent.clientId.id})? $pHasLowerClient, less than q's (${qIdent.clientId.id})? $qHasHigherClient")

          // B: There is no gap in pos, so we can only insert if our client is
          // strictly between p's and q's clients, AND pIdent.pos is > minPosValue,
          // since we require that new Identifiers fulfil this requirement.
          if (posDiff == 0 && pInvariant5 && pHasLowerClient && qHasHigherClient) {
            //println(s"B succeeded")
            r :+ Identifier(pIdent.pos, clientId)

          // C: There is a gap of 1 in pos, so we can use p's position if our
          // client is higher, AND pIdent.pos is > minPosValue,
          // since we require that new Identifiers fulfil this requirement...
          } else if (posDiff == 1 && pInvariant5 && pHasLowerClient) {
            //println(s"C succeeded")
            r :+ Identifier(pIdent.pos, clientId)

          // D: ...or use q's position if our client is lower than q's. We know that
          // qIdent.pos > minPosValue since posDiff is one, so
          // qIdent.pos = pIdent.pos + 1, and pIdent.pos >= minPosValue
          } else if (posDiff == 1 && qIdent.pos > minPosValue && qIdent.pos < maxPosValue && qHasHigherClient) {
            //println(s"D succeeded")
            r :+ Identifier(qIdent.pos, clientId)

          // No space to insert a new ident at this position
          // so recurse with pIdent appended to r. We know this gives r >= p since
          // r is a prefix of p, and similarly r <= q
          // NOTE: This is also a special case for q: we have know that pIdent < qIdent.
          // Therefore we have no maximum imposed by q on later digits - instead of
          // recursing with q' = rest(q) we instead just use nil to remove restrictions
          // on maximum. This is likely to allow us to
          } else {
            //println(s"No gap on client ids, proceeding to prefix ${r :+ pIdent}")
            positionBetweenRec(rest(p), Nil, r :+ pIdent, clientId)
          }
        }

      // pIdent >= qIdent, no space to insert a new ident between them
      // so recurse with pIdent appended to r. This will make r > p when w
      // find the index where we can add an index greater than p's, since all
      // all idents up to that point will be equal to p's.
      // We also know that all prefixes of p are <= q since p < q
      case _ =>   //case 0 if we have 1 separately below
        //println(s"Idents equal, proceeding to prefix ${r :+ pIdent}")
        positionBetweenRec(rest(p), rest(q), r :+ pIdent, clientId)
    }
  }

  private[logoot] def positionBetween(p: Position, q: Position): DeltaIO[Position] = {
    for (
      deltaId <- getDeltaId
    ) yield {
      val clientId = deltaId.clientId
      positionBetweenRec(p, q, Nil, clientId)
    }
  }

}
