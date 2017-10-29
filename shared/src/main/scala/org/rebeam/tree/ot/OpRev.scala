package org.rebeam.tree.ot

/**
  * An operation with a revision index
  * Conceptually all documents (lists) in OT system are created as an empty
  * list with no operations applied.
  * Documents are then edited by applying a sequence of operations,
  * this builds a history which is just a list of the operations in order.
  * The rev of each operation is just its index within this list.
  * This also allows us to use the rev to describe the document - rev i of the
  * document is produced by applying i of the operations in the history, so
  * rev 0 is always the empty list, rev1 is given by applying the first op with
  * rev 0, and so on.
  * @param op   The operation
  * @param rev  The revision of the operation, which is also the document revision
  *             to which this operation must be applied, yielding document revision
  *             rev + 1.
  * @tparam A   The type of element in the list operated on
  */
case class OpRev[A](op: Operation[A], rev: Int)
