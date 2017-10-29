package org.rebeam.tree.ot

/**
  * The state of a server handling operations on a list of elements.
  * State starts from an empty list, and an empty history.
  * history(0) produces revision 0 of the list when applied. There
  *
  * @tparam A       The type of element in the list
  * @param list     The current state of the list, after all operations have been applied
  *                 Note that this is redundant - we can recover it by applying history
  *                 to the empty list, but stored here for efficiency.
  * @param history  A list of all operations that have been applied, in application order
  *                 Note that we must always have all operations, starting from revision
  *                 0 and using consecutive revision indices, so only operations are stored,
  *                 and each has a revision index equal to its index in the history.
  */
case class ServerState[A](list: List[A], history: List[Operation[A]]) {

  /**
    * Produce a ServerState that has been updated with an operation applied by a client on the list at specified
    * revision. Note that the client might have been out of date, in which case the operation must be transformed
    * to apply on the up to date list.
    * Also produces the transformed operation that was applied to the up to date list.
    * @param clientOpRev  The operation from the client, and the revision it produced on client when applied.
    * @return             A pair with new ServerState, and the transformed OpRev we applied
    */
  def updated(clientOpRev: OpRev[A]): (ServerState[A], OpRev[A]) = {
    val rev = clientOpRev.rev
    val clientOp = clientOpRev.op
    require(rev >= 0, "Revision index must be >= 0")
    // Each operation in history operates on the document revision matching its index
    // in history, and produces that document revision + 1. Hence `<=`
    require(rev <= history.size, s"Revision index ($rev) must be <= most recent revision (${history.size})")

    // The client op is at given rev, so operates on a document with this many
    // ops from history applied already. We drop these from history, leaving
    // only ops that the server applied AFTER this document state.
    val postOps = history.drop(rev)

    // We want to know what op will change the current list to reach the same
    // result that would be achieved by taking the client revision of the list,
    // then applying clientOp, then all of postOps in order
    // (transformed to apply after clientOp).
    //
    // The following diagram confusingly represents this. l is the initial list,
    // at the revision on which client op was performed. c is the client op.
    // pi is the ith postOp (i.e. server operation AFTER the client revision).
    // n is the final result of all the operations, the new list for the new
    // server state. Note that `after` is just a convenience method for
    // Operation.transform.
    //
    // Each position left to right represents a new state of the list. Moving
    // up represents applying the client op, or an operation transformed to
    // do the same thing after some other ops. Moving down represents applying
    // an op based on a server (post) op.
    //
    //    *
    //  /c \p0.after(c)
    // l    \p1.after(p1.after(c))
    //  \p0  \p2.after(p2.after(p1.after(c))
    //   \p1  n
    //    \p2/c.after(p0).after(p1).after(p2)
    //      *
    //
    // The top branch represents applying c, then applying p0 transformed to be
    // applied after c, then p1 transformed to be applied after THAT transformed
    // operation based on p0, etc. until we have applied all post ops. This is
    // what the client would get by applying its operation first, then the
    // server ops transformed appropriately to account for c.
    //
    // The branch we are more interested in is the bottom branch, where we work out
    // what operation needs to be applied after p0, p1 and p2, to get the same
    // end result n - this is c.after(p0).after(p1).after(p2). To see how we reach
    // this, consider step by step:
    //
    // After just p0 has been applied, this would just be c.after(p0):
    //
    //    *
    //  /c \p0.after(c)
    // l    l0
    //  \p0/c.after(p0)
    //    *
    //
    // This gets us to l0, we have moved one step closer to n.
    //
    // For the next step we need another diamond, but this time l is replaced by
    // l0, c is replaced by c.after(p0), and p0 is replaced by p1. This yields
    // c.after(p0).after(p1) in the lower right transformation. We can continue
    // until we reach n, with c.after(p0).after(p1).after(p2) as stated.
    //
    val postClientOp = postOps.foldLeft(clientOp){case (op, postOp) => op.after(postOp)}

    // New state and the new op that produced it
    (
      // Apply the op to out list, and add it to our history
      ServerState(postClientOp(list), history :+ postClientOp),

      // We have added postClientOp at the end of history with size n, so
      // it is the nth element (zero based)
      OpRev(postClientOp, history.size)
    )

  }
}
