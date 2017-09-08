package org.rebeam.tree.logoot

import org.rebeam.tree.Delta.DeltaIO
import org.rebeam.tree.Delta._
import org.rebeam.tree.sync.Sync.{ClientId, Guid, HasId}

import scala.collection.immutable.SortedMap
import scala.collection.mutable.ListBuffer

/**
  * Implementation of the Logoot CRDT based on https://hal.archives-ouvertes.fr/inria-00432368/document
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
  */
object Logoot {

  // The maximum value of pos field of an Identifier. We use pos values from 0 to this value, inclusive
  val maxPosValue: Long = Int.MaxValue

  // We can construct a (large!) number by taking the pos of each Identifier in a list
  // as a digit of a number in this base. In other words there are `base` options per identifier,
  // from 0 to maxPosValue
  val base: Long = maxPosValue + 1

  /**
    * The first logical Position in all Sequences.
    * Not actually present in the Sequence, but used by the logic for insertion of new entries.
    */
  val beginningPosition = Position(List(Identifier(0, ClientId(-1))))

  /**
    * The last logical Position in all Sequences.
    * Not actually present in the Sequence, but used by the logic for insertion of new entries.
    */
  val endPosition = Position(List(Identifier(maxPosValue.toInt, ClientId(-1))))

  /**
    * An identifier
    * @param pos  An integer
    * @param site A site identifier - it is assumed that each client has a single copy
    *             of each Document. This is reasonable since all editing with a client
    *             is synchronous. Multiple clients could be used for asynchronous editing
    *             if required.
    */
  case class Identifier(pos: Int, site: ClientId)

  /**
    * Compare Identifiers by pos, and then by site (using ClientId's id value)
    */
  implicit val identifierOrdering: Ordering[Identifier] = new Ordering[Identifier] {
    def compare(a: Identifier, b: Identifier): Int = {
      val byPos = a.pos.compareTo(b.pos)
      if (byPos == 0) {
        a.site.id.compareTo(b.site.id)
      } else {
        byPos
      }
    }
  }

  /**
    * A position is a list of Identifiers
    * @param identifiers  The list of identifiers
    */
  case class Position(identifiers: List[Identifier]) extends AnyVal {
    override def toString: String = "Position(" + identifiers.map(id => s"${id.pos}").mkString(" ") + ")"
  }

  object Position {
    val empty = Position(Nil)
  }

  /**
    * Compare Positions by their identifiers, in order, for the indices
    * present in both Positions. If all are equal, compare by number of
    * identifiers.
    */
  implicit val positionOrdering: Ordering[Position] = new Ordering[Position] {
    override def compare(x: Position, y: Position): Int = {
      val xe = x.identifiers.iterator
      val ye = y.identifiers.iterator

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
    * @param entries  The sorted map from position ids to entries
    * @tparam A       The type of value in entries
    */
  case class Sequence[A](entries: SortedMap[PositionId, A]) {

    lazy val values: List[A] = entries.values.toList
    lazy val pids: List[PositionId] = entries.keys.toList

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
          (beginningPosition, endPosition)
        } else if (i == 0) {
          (beginningPosition, pids.head.position)
        } else if (i == pids.size) {
          (pids.last.position, endPosition)
        } else {
          (pids(i - 1).position, pids(i).position)
        }

      positionsBetween(p, q, 1).map(_.head)
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
      deleted(pid.id).copy(entries.updated(pid, a))

    /**
      * Create a Sequence with an entry deleted. If there
      * is no entry with a positionId matching the guid then
      * result is the same sequence.
      * @param guid The guid of the PositionId for the entry
      * @return     A sequence with any matching entry removed
      */
    def deleted(guid: Guid[PositionId]): Sequence[A] =
      copy(entries -- entries.keySet.filter(_.id == guid))

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
      val oldEntries = entries.filter(e => e._1.id == pid.id)
      oldEntries.headOption match {
        // There are no old entries, so nothing to move, sequence unaltered
        case None => this

        // There is at least one old entry. Delete all old entries,
        // and move the first old entry's value to the new pid
        case Some((_, a)) =>
          val oldEntriesDeleted = entries -- oldEntries.keys
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

  private[logoot] def nextPos(it: Iterator[Identifier]): Int = if (it.hasNext) it.next().pos else 0

  /**
    * Add an offset to a position.
    * For efficiency, we treat the position r as a sequence of ints as usual,
    * but the offset is a plain long value.
    * r is considered as the integer represented by a string of digits
    * in base `base`, with most significant digit first, e.g. for a three
    * element list, r(0) * base * base + r(1) * base + r(0).
    * offset is then just a plain scala value.
    * @param offset Offset to add to r
    * @param r      r, a position, representing a number in base `base`
    */
  private[logoot] def addToPos(offset: Long, r: ListBuffer[Int]): Unit = {
    // Work backwards from last (least significant) digit in r
    // We need to handle "carry" as we go
    // At each iteration, offset is in the same base as the ith digit
    // of r
    var i = r.size - 1
    var o: Long = offset
    while (o > 0) {

      // Add current digit of r to the offset
      o += r(i)

      // The last "digit" of o is now set into the current pos digit
      r(i) = (o % base).toInt

      // Shift offset by one digit in base
      o /= base

      // Next (more significant) digit of r
      i = i - 1

    }
  }

  /**
    * Construct a new position, with size identifiers (digits),
    * between positions p and q, at a given offset from p, and
    * using given site.
    * @param size     The number of identifiers (digits) in the result.
    * @param offset   The difference between p and result, when considering both as
    *                 numbers in base `base`, with p padded with 0s as appropriate.
    * @param p        First position
    * @param q        Second position
    * @param site     Site for new position (used in all new identifiers, and at least last identifier)
    * @return         A new Position
    */
  private[logoot] def constructPosition(size: Int, offset: Long, p: Position, q: Position, site: ClientId): Position = {
    val r = ListBuffer.empty[Int]

    //First make "prefix" of the positions in p - extending with 0 position as necessary
    val pe = p.identifiers.iterator
    for (_ <- 0 until size) {
      r.append(nextPos(pe))
    }

    //Add the offset
    addToPos(offset, r)

    // Pos values are worked out, construct the Position by filling out site values

    // The paper doesn't seem to explain the logic behind assigning the sites this
    // way, it seems to be as follows.
    // When assigning the identifiers' pos values we ensure that they lie between
    // p and q, considering only the pos values. However when comparing two Positions
    // for ordering them, we also consider the site values in each identifier.
    // Therefore we need to make sure that an identifier-by-identifier comparison
    // of the new position r to p and q still places it between them.
    // First consider all positions - we know that except for the implicit
    // first position, which has a single pos value as the minimum value (we will
    // call this 0 from now on for simplicity) no position has a pos of value 0. This
    // is because all positions are constructed either by retaining the number of digits
    // of their lower neighbour and incrementing one or more digits, or by adding a digit
    // to give a base position and then adding at least 1
    // Looking at any identifier, and assuming it isn't the last;
    //  1.  if it is present in both p and q, then we know that pos p <= pos q.
    //      If pos p < pos q, then site doesn't matter, and if pos p == pos q
    //      then we know site p <= site q. So we can choose site p, so the new
    //      identifier in r is >= that in p and <= that in q.
    //  2.  If the identifier is NOT present in p, but is present in q
    //      then r and q are both longer than p, and p will be compared as
    //      if it had a 0 pos. Therefore since
    //



    val ids = ListBuffer.empty[Identifier]
    for (i <- 0 until size) {
      val ri = r(i)
      // Last site is as specified
      val s = if (i == size - 1) {
        site

      // Site from p if pos matches p
      } else if (i < p.identifiers.size && ri == p.identifiers(i).pos) {
        p.identifiers(i).site

      // Or from q if pos matches q
      } else if (i < q.identifiers.size && ri == q.identifiers(i).pos) {
        q.identifiers(i).site

      // Or default to site
      } else {
        site
      }
      ids.append(Identifier(ri, s))
    }

    // Done!
    Position(ids.toList)
  }

  private[logoot] def positionsBetween(p: Position, q: Position, n: Int): DeltaIO[List[Position]] = {
    // TODO pure implementation
    var size: Int = 0
    var diff: Long = 0
    val pe = p.identifiers.iterator
    val qe = q.identifiers.iterator

    val maxSize = Math.max(p.identifiers.length, q.identifiers.length) + 2

//    println(s"generate $n positions from:\n $p to:\n $q")

    // Extend the prefix size we will use until that prefix of p and q are separated by at least
    // n possible new positions, implying their difference is > n.
    // Note we only allow an Int number of new entries, and use a Long for diff, so we
    // won't overflow. The worst case is that n is integer max value, mv, and for some
    // reason diff is also mv at `while` check, so iteration continues. We then multiply diff
    // by base, which is mv + 1, to get mv*(mv+1). Then we could have nextPos(qe) as mv, and
    // nextPos(qe) as 0, so diff has mv added to give mv*(mv + 1) + mv. This is now
    // definitely bigger than n (which is up to mv), so we stop iteration with:
    // diff = mv^2 + 2mv.
    // mv is 2^31 - 1, so round it up to 2^31, and we have:
    // diff = 2^62 + 2^32
    // which is less than max long value of 2^63 - 1
    while (diff <= n) {
      size += 1
      diff *= base
      diff += nextPos(qe) - nextPos(pe)
      if (size > maxSize) sys.error("positionsBetween has inadequate diff between p and q - must be sorted wrongly from sites")
//      println(s"prefix size $size gives diff $diff")
    }

    // We can use offsets from 1 to count, (count total options)
    // where count is diff - 1
    // Since we required diff > n we have count >= n.
    // e.g. looking at a single digit,
    // if we have q = 5, p = 2, we get diff = q - p = 3.
    // This gives count = 2, since we have two usable
    // options, 3 and 4. These are given by p+1 = 3 and p+2 = 4,
    // so offset is from 1 to count = 2, inclusive.
    val count: Long = diff - 1

    // We can use a window of step options for each new position
    val step: Long = count / n
//    println(s"step size $step")

    for (
      deltaId <- getDeltaId
    ) yield {
      // Generate the n positions
      val positions = new ListBuffer[Position]()
      var offset: Long = 0
      for (j <- 0 until n) {
        val random: Long = 1 // TODO use pseudo-random value from 1 to step, inclusive - need to add random generator to DeltaIO
        positions.append(constructPosition(size, offset + random, p, q, deltaId.clientId))
        offset += step
      }

      // Done
      positions.toList
    }
  }

}
