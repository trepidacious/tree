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
  * Note that I still feel like it should be possible to just have Ints rather than identifiers, with a single
  * "site index" as a Guid paired with the ints. This would still give uniqueness, but a different ordering of lines
  * when integrating from different clients - perhaps the multiple site indices give better preservation of intent?
  * For example when a position id is extended, any position ids using that position with the original site identifier
  * plus additional appended identifiers will stay together in the document. But in a presumably common case where
  * positions with different pos and site indices are at the same level, edits from different sites will be interleaved.
  */
object Logoot2 {

  /**
    * The minimum value of pos field of an Identifier. We use pos values from this value to maxValue, inclusive
    */
  val minPosValue: Int = 0

  /**
    * The maximum value of pos field of an Identifier. We use pos values from minValue to this value, inclusive
    */
  val maxPosValue: Int = Int.MaxValue

  /**
    * The first/minimum position. Logically considered to be present in sequence when
    * inserting before the first element. The single identifier is also used
    * to pad positions we are inserting after.
    * ClientId value is not important for the valid operation of the Logoot. However 0
    * is conventionally used for initial data, and >= 1 for actual clients.
    */
  val firstPosition = Position(Identifier(minPosValue, ClientId(0)))

  /**
    * The last/maximum position. Logically considered to be present in sequence when
    * inserting after the first element. The single identifier is also used
    * to pad positions we are inserting before.
    * ClientId value is not important for the valid operation of the Logoot. However 0
    * is conventionally used for initial data, and >= 1 for actual clients.
    */
  val lastPosition = Position(Identifier(maxPosValue, ClientId(0)))

  /**
    * An identifier
    * @param pos  An integer
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
    def apply(xs: Identifier*) = List.apply()
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

  /**
    * This recursively implements an algorithm where we increment an index i, building
    * a prefix of the new position r by taking digits from p, until we reach an identifier
    * pair in p and q where there is a gap to insert a new identifier between those of p
    * and q - at this point we return the prefix plus this new identifier (which uses the
    * specified clientId).
    *
    * While we are working in the range of indices where p and q have ids, we know that the
    * result will be > p because it uses a prefix of p, until a new identifier is used
    * that is greater than p's. It is < q for the same reason - each identifier of p is less than the
    * corresponding one from q.
    *
    * If we run out of ids in p or q we have additional considerations, since we use the minimum identifier
    * to pad p, and the maximum one to pad q.
    *
    * We know that when using padding we are still > p since while we are in p we will continue
    * until we can produce a greater ident in a position, and when we move beyond p we are building
    * a longer position with a shared prefix. This is true regardless of the contents of q.
    *
    * We therefore just need to consider whether we will be less than q when using padding.
    *
    * If reach the end of p before the end of q we will pad using minPosValue, which gives an
    * ident <= any other ident. We must eventually find an id in q where the minimum identifier
    * is less than the identifier from q for the reasons set out below, and this allows us to generate
    * a prefix that is < q.
    *
    * If we reach the end of p and q at the same id, then we know that an id in p must have been
    * less than that in q, otherwise p would not be less than q. Therefore we are free to add
    * any id subsequently, and still generate a position strictly between p and q (longer than p,
    * and having an identifier < that in q, before the identifiers we add).
    *
    * If we reach the end of p AFTER the end of q, then again we know that the prefix of p of the
    * same length as q contained an id less than that in q, so the same logic applies as for p and
    * q of the same length.
    *
    * Do we always generate positions that have both a position above them, and one below them?
    * We can always generate a position above, by just appending an identifier, and this algorithm
    * will do that by just increasing the prefix - it may reach an existing id of p where it
    * can increase that id, or in the worst case it will exhaust p and append another digit to
    * be above p.
    *
    * The below case is more complex. To generate a position < q, we need to produce a string of
    * identifiers which are always <= those of q, followed by an id that is less q. The id that
    * is less than q obviously needs to be within the length of q, to give an entire position < q.
    * If we have p of same or greater length than q, q must contain the required id, since p < q,
    * and so we use this id and are done.
    * If p is shorter than q, then on the subsequent identifiers that are NOT in p, we will pad p with
    * (minPosValue, C) where C is the ClientId chosen for the firstPosition. Therefore we
    * need to know that in ALL cases (minPosValue, C) is <= the id from q, and that in at least one
    * identifier in q, (minPosValue, C) is < that identifier from q.
    * This can be ensured by requiring C < the site from q, however we do not want to do this
    * since it places a difficult constraint on client ids - we can reserve a special id for this
    * application, but it could be easily forgotten somewhere, breaking the system.
    * Therefore we will assume C is arbitrary and clientIds coming from actual clients are also
    * arbitrary, and may be less than, equal to or greater than C. We will then set out to still
    * ensure that all ids in the system are >= (minPosValue, C)
    *
    * All the ids in the system come from one of the following:
    *   1. "Copy" of an existing id, generated by using a prefix of p. If existing id complies, copy will too!
    *   2. (minPosValue, C), either in the firstPosition implicitly at the start of the sequence and used
    *      as p, or from padding p.
    *   3. (maxPosValue, C), either in the lastPosition implicitly at the end of the sequence and used as q,
    *      or from padding q.
    *   4. A new id generated by positionBetweenRec, using a pos value chosen by positionBetweenRec and the
    *      clientId of the site generating the position. The code where these are generated is labelled A, B, C, D.
    *
    * Of these, 1 is self-evident, 2 and 3 are >= (minPosValue, C) by inspection, and we just need to ensure that
    * 4 is valid - this is done simply by ensuring that we only generate new ids with a pos value > minPosValue.
    * Note that we also ensure that the pos value is < maxPosValue (we could maybe use <= maxPosValue in some cases,
    * but this isn't important) - this allows us to show that all pos values are valid later.
    *
    * So this covers the requirement that ALL ids are >= (minPosValue, C) (i.e. a NON-STRICT inequality).
    *
    * We now just need to know that there must be at least one id in q where (minPosValue, C) < id in q
    * (i.e. the STRICT inequality). We know this because q must be one of two things, either the implicit
    * endPosition (maxPosValue, C) which is definitely greater than (minPosValue, C) or a position generated
    * by positionBetweenRec below. This ends with an identifier in class 4 above, where we ensure that
    * pos > minPosValue, so id > (minPosValue, C) - this gives us at least one id within q where we can
    * produce a lower identifier by using minPosValue. Note that q cannot be the implicit firstPosition since
    * this is < all other positions.
    *
    * Finally, we can see that we always have valid pos values in all ids by looking at id sources 1 to 4 above
    * and noting that each one always uses a pos value from minPosValue to maxPosValue inclusive.
    *
    * @param p          We will generate a position > p
    * @param q          We will generate a position < q
    * @param r          The prefix of result already generated - we will add identifier(s) to this to produce result.
    * @param clientId   The clientId to be used for the final identifier in the result
    * @return           A Position > p and < q, and with specified clientId in the final identifier
    */
  @tailrec
  private[logoot] def positionBetweenRec(p: Position, q: Position, r: Position, clientId: ClientId): Position = {

    // We need a permissive tail that is empty if list is empty, so we can
    // trim down p and q as we recurse
    def rest(pos: Position): Position = if (pos.isEmpty) Nil else pos.tail

    // Use the head of p and q, if they are empty then pad with firstPosition or lastPosition respectively
    // These are used as the limits on the range of the new ident added to r to produce a result or to
    // recurse.
    val pIdent = p.headOption.getOrElse(firstPosition.head)
    val qIdent = q.headOption.getOrElse(lastPosition.head)


    identifierOrdering.compare(pIdent, qIdent) match {
      // pIdent < qIdent - we can try to insert at this digit
      case -1 =>
        val posDiff = qIdent.pos - pIdent.pos

        // We have a gap in pos, so choose a random pos in that gap.
        // We lie between p and q based on pos only, so can use our own site
        if (posDiff > 1) {
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

          // B: There is no gap in pos, so we can only insert if our client is
          // strictly between p's and q's clients, AND pIdent.pos is > minPosValue,
          // since we require that new Identifiers fulfil this requirement.
          if (posDiff == 0 && pIdent.pos > minPosValue && pHasLowerClient && qHasHigherClient) {
            r :+ Identifier(pIdent.pos, clientId)

          // C: There is a gap of 1 in pos, so we can use p's position if our
          // client is higher, AND pIdent.pos is > minPosValue,
          // since we require that new Identifiers fulfil this requirement...
          } else if (posDiff == 1 && pIdent.pos > minPosValue && pHasLowerClient) {
            r :+ Identifier(pIdent.pos, clientId)

          // D: ...or use q's position if our client is lower than q's. We know that
          // qIdent.pos > minPosValue since posDiff is one, so
          // qIdent.pos = pIdent.pos + 1, and pIdent.pos >= minPosValue
          } else if (posDiff == 1 && qHasHigherClient) {
            r :+ Identifier(qIdent.pos, clientId)

          // No space to insert a new ident at this position
          // so recurse with pIdent appended to r. We know this gives r >= p since
          // r is a prefix of p, and similarly r <= q
          } else {
            positionBetweenRec(r :+ pIdent, rest(p), rest(q), clientId)
          }
        }

      // Identifiers are equal, no space to insert a new ident between them
      // so recurse with pIdent appended to r. We know this gives r >= p since
      // r is a prefix of p, and similarly r <= q
      case 0 =>
        positionBetweenRec(r :+ pIdent, rest(p), rest(q), clientId)

      // Code error - positions are meant to be sorted
      case 1 =>
        sys.error("q > p when generating position!")
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
