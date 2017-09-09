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
    *   new positions before the first generated position. Similarly there is one of class L implicitly at the
    *   end of each sequence.
    *   4. All generated positions have a final identifier with a clientId from the client that generated them.
    *   5. All generated positions have a final identifier with a pos strictly > minPosValue and < maxPosValue.
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
    * position meeting the invariants and requirements, to allow a new entry to be inserted into a Sequence.
    *
    * This can be expressed recursively. At each recursion we term the previous recursion's result l
    * (empty on first recursion), and choose a new ident ri to form the result of this recursion, r = l + ri. On
    * the final recursion this yields the final position f.
    *
    * Lets define an operator <|, where p <| r means that either p < r, or r is a prefix of p (including the option of
    * being empty, or the same length as and so identical to p).
    *
    * To produce the final result, we need r to meet all invariants and the condition p < r < q. When we cannot do
    * this in an intermediate recursion, we instead ensure we meet an easier condition p <| r <= q. This obviously
    * means that for all recursions, we have p <| l <= q.
    *
    * Note that when we choose to recurse, we know that ri will not be the final identifier in a generated position,
    * and so invariants 4 and 5 do not apply, which also eases the choice of ri.
    *
    * At each recursion step we perform three steps:
    *
    *   1. Produce a top identifier ti and bottom identifier bi satisfying invariant 1; i.e. they are not necessarily
    *      suitable as final identifiers in a generated sequence. These are chosen so that:
    *
    *        a. ri == bi      => p <| r <= q    // i.e. we can recurse with ri = bi
    *        b. bi < ri < ti  => p < r < q      // i.e. we can use bi and ti to find ri that produces final result f
    *
    *      where => is implication. (Since we don't have access to pi and qi after we have recursed outside the
    *      range of indices of the respective position, we instead use a proxy for pi and qi that DOES exist at all i).
    *
    *   2. Check whether there exists ri s.t. bi < ri < ti and satisfying invariants for the the ident itself and
    *      the sequence produced. If so this implies p < r < q and we are finished.
    *
    *   3. If 2 fails, choose ri = bi, so that a. implies p <| r <= q, and so we will reach a solution at later
    *      recursion.
    *
//    * We now need to show that p <| r <= q (and p < q) is sufficient to ensure that there is a longer
//    * position r ++ n for some possibly empty list of idents n, where p < r ++ n < q, and that the algorithm above
//    * will actually choose it.
//    *
//    * We start from knowing that p < q. This implies that either
//    *   1. There is an index i where pi < qi.
//    *      At this index we will choose bi = pi and ti = qi (see step 1 below)
//    *     a. If we have any of the terminating cases A to D from step 2, we will choose ri s.t. pi < ri < qi.
//    *        We know p <| l =< q so either l is a prefix of p and l + ri > p, or l > p, so l + ri > p.
//    *        We know l <= q, so l + ri < q.
//    *        Therefore in this case we know that we have a valid solution f.
//    *     b. If we do NOT have any of the terminating cases (i.e. pi < qi, but with no identifier between pi and qi)
//    *        then we will recurse, using ri = bi (see step 3), and we know bi = pi from step 1 below.
//    *        Hence r = l + pi. But since pi < qi, this means that any position having r as a prefix is < q; we
//    *        have ensured that all remaining recursions will fulfil r < q.
//    *        Therefore we only require that remaining recursions fulfil r > p; any identifiers will do this, they
//    *        just need to be valid.
//    *
//    *
//    *        We know that p <| r <= q for the next step, but actually in this case we know more -
//    *        we now know that r and any position with it as a prefix is strictly < q, regardless of length, since
//    *        the identifier pi is < qi.
//    *
//    *
//    *
//    *   2. p is a prefix of q, and q is longer.
//    *
//    * CONTINUE
//    *
//      * This is the necessary and sufficient condition for there to exist a sequence r + n that fulfils p < r ++ n,
//      * where n may be empty.
//      *   p <| r is sufficient because either r is already > p, or it is a prefix to which we can add the remainder of p
//      *   and then any valid identifier to give a sequence > p.
//      *   p <| r is necessary. Consider if p <| r is not true. In that case p >= r, and r is not a prefix of p. If r is
//      *   not a prefix of p then there exists some first index i where pi != ri. But p >= r, so we know that we cannot
//      *   have ri > pi, so the only remaining inequality is that ri < pi, and so r < p, and for all n, r + n < p as well.
//      *
//      * r <= q is necessary for there to exist n s.t. r ++ n < q, if r > q then all r ++ n are also greater than q.
//      * p <| r <= q and p < q is sufficient for there to exist n s.t. p < r ++ n < q.
    *
    *
    * ## Step 1 - produce bi and ti
    *
    * At each recursion i, there are three interesting booleans:
    *   inP = i < p.size (i.e pi exists)
    *   inQ = i < q.size (i.e. qi exists)
    *   lessThanQ, which is true iff the prefix l passed to this recursion is less than Q with any suffix (because it
    *   contains some identifier less than the corresponding identifier in q)
    *
    * We choose bi and ti according to:
    *
    * if (inQ && !lessThanQ) ti = qi else ti = lastPosition.head
    * if (inP) bi = pi else bi = min(firstPosition.head, ti)
    *
    * We need to show that conditions a and b from above apply, note we have rewritten the conditions in terms of bi
    * and ti:
    *
    * Condition a is that p <| l + bi <= q
    *
    * First we show p <| l + bi. We know that p <| l, so either:
    *   1. p < l so that we must have p < l + bi for any bi, satisfying p <| l + bi
    *   2. l is a prefix of p, and either
    *      a. inP is true, then bi = pi, so l + bi is also a prefix of p, satisfying p <| l + bi
    *      b. inP is false, then l must be the whole of p, so l + bi > p, satisfying p <| l + bi
    *
    * Then we show l + bi <= q. We know that l <= q, and either:
    *   1. inP is true, and bi = pi. Note also that inP must have been true for all previous
    *      recursions, and since we reached stage i they did not return results - they recursed
    *      by appending bj at each stage j. So we know l is just the prefix of p up to i-1. Therefore
    *      l + bi is just the prefix of p up to i. This must be <= q, otherwise we would have p > q.
    *   2. inP is false, and bi = min(firstPosition.head, ti). Either:
    *      a. (inQ && !lessThanQ), so ti = qi, and bi = min(firstPosition.head, qi)
    *         TODO
    *      b. !(inQ && !lessThanQ), so ti = lastPosition.head, and bi = min(firstPosition.head, lastPosition.head),
    *         so bi = firstPosition.head.
    *         The condition means that either or both of the below apply:
    *         i. !inQ. So neither pi nor qi exists at this i. Therefore l contains all the digits chosen for indices j
    *            where pj and/or qj existed. Either:
    *              One. p is shorter than q. There was a first integer j where inP was false and inQ was true.
    *         ii. or lessThanQ. This means that l plus any suffix is less than q, satisfying l + bi <= q
    *
    *
    *
    * If inP is true:
    *   We will choose bi = pi. Now skip ahead to step 3 - this states that when we recurse, we always choose
    *   ri = bi ( = pi). This implies that the prefix at any recursion where inP is true is just the prefix of p.
    *   So we can see that if ri > bi (=pi) implies that r will be a prefix of p then a
    *   If inQ is also true, we choose ti = qi. This gives
    *
    * ## Step 2 - check for solution f
    *
    * We are producing a final identifier of a generated position, so we must comply with invariants 4 and 5.
    * Invariant 1 must be fulfilled by all identifiers including this one.
    *
    * Invariants 2 and 3 are fulfilled automatically, and Invariant 4 is fulfilled just by using the provided
    * clientId in all cases. Fulfilling invariant 5 gives invariant 1 for free, so we only need to worry about
    * invariant 5 and ensuring that bi < ri < ti.
    *
    * Note that when considering bi and ti, we can only rely on Invariant 1, but this is sufficient.
    *
    * We recognise 4 situations where such a final identifier exists (which may not be exhaustive), and which
    * are used in this order of preference:
    *   A. bi.pos and ti.pos are not consecutive, i.e. there is at least pos value strictly between them.
    *      We know from invariant 1 that bi.pos >= minPosValue and ti.pos <= maxPosValue, so all pos values
    *      between them meet invariant 5. We pick a pos s.t. bi.pos < pos < ti.pos according to a pseudo-random
    *      sequence that is provided to the function (TODO). This gives bi < ri < ti since identifiers are compared
    *      first by pos.
    *   B. bi.pos == ti.pos, bi.pos fulfils invariant 5, and bi.clientId < clientId < ti.clientId. In this case
    *      we can use pos = bi.pos (fulfilling invariant 5). This gives bi < ri < ti since the comparisons
    *      on pos are all equal, so we then compare by clientId giving desired result.
    *   C. bi.pos and ti.pos are consecutive, bi.pos fulfils invariant 5, and clientId > bi.clientId. In this case
    *      we can use pos = bi (fulfilling invariant 5). This gives bi < ri since bi.pos == ri.pos and
    *      bi.clientId < ri.clientId. We have ri < ti since ri.pos is one less than ti.pos (bi.pos and ti.pos
    *      consecutive)
    *   D. bi.pos and ti.pos are consecutive, ti.pos fulfils invariant 5, and clientId < ti.clientId. This is the same
    *      as C, except that we use pos = ti.pos (fulfilling invariant 5). This gives bi < ri since
    *      bi.pos < ri.pos == ti.pos. We have ri < ti since ri.pos == ti.pos and ri.clientId < ti.clientId.
    *
    * Therefore all cases A to D produce an ri meeting invariants and bi < ri < ti. These inequalities in turn imply
    * that p < r < q as required to yield a result.
    * If A - D all fail we will move on to step 3 and recurse.
    *
    *
    * ## Step 3 - recurse
    *
    * Already described - just choose ri = bi.
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
    // except that we use qIdent instead of the firstPosition iff it is lower.
    // These are used as the limits on the range of the new ident added to r to produce a result or to
    // recurse.
    val qIdent = q.headOption.getOrElse(lastPosition.head)
    val pIdent = p.headOption.getOrElse(minIdent(firstPosition.head, qIdent))

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

//      // Code error - on the first ident where p is < q we stop using q's idents
//      // directly and instead move to max idents. This means that p's idents cannot
//      // be > q's idents after this. So for this case to occur we need p to have an ident
//      // higher than q's BEFORE it has one lower than q's, implying p > q.
//      case 1 =>
//        sys.error("p's ident > q's ident when generating position, implying p > q")
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
