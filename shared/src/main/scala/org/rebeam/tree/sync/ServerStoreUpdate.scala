package org.rebeam.tree.sync

import org.rebeam.tree.sync.Sync._
import DeltaIORun._
import org.rebeam.tree.Delta

sealed trait ServerStoreUpdate[U, A, D <: Delta[U, A]] {
  def append(e: ServerStoreUpdate[U, A, D])(implicit refAdder: RefAdder[U, A]): ServerStoreUpdate[U, A, D]
}

object ServerStoreUpdate {
  /**
    * Generated to set a complete new model. The first update to an observer
    * will always be a ServerStoreFullUpdate
    * @param modelAndId Model and id
    */
  case class ServerStoreFullUpdate[U, A, D <: Delta[U, A]](modelAndId: ModelAndId[A]) extends ServerStoreUpdate[U, A, D] {
    def append(e: ServerStoreUpdate[U, A, D])(implicit refAdder: RefAdder[U, A]): ServerStoreUpdate[U, A, D] = {
      e match {
        // Full 1 + full 2 = full 2
        case f@ServerStoreFullUpdate(_) => f

        // Full 1 + inc 1 = new full update with model from full 1 update with deltas from inc 1
        case i@ServerStoreIncrementalUpdate(_, _, _) =>
          val updatedModel = i.deltas.foldLeft(modelAndId.model){
            case (m, dic) => {
              val result = dic.runWith(m)
              refAdder.addRefs(result)
            }
          }
          ServerStoreFullUpdate(ModelAndId(updatedModel, i.updatedModelId))
      }
    }
  }

  /**
    * Called each time the ServerStore's model changes due to one or more
    * deltas
    * @param baseModelId      The id of the model before the updates
    * @param deltas           The deltas that have been applied, in the order
    *                         they were applied, with JSON encoding and context for execution
    * @param updatedModelId   The id of the model after the updates
    */
  case class ServerStoreIncrementalUpdate[U, A, D <: Delta[U, A]](baseModelId: ModelId,
                                             deltas: Seq[DeltaWithIC[U, A, D]],
                                             updatedModelId: ModelId) extends ServerStoreUpdate[U, A, D] {
    def append(e: ServerStoreUpdate[U, A, D])(implicit refAdder: RefAdder[U, A]): ServerStoreUpdate[U, A, D] = {
      e match {
        //Inc 1 + full 1 = full 1
        case f@ServerStoreFullUpdate(_) => f

        //Inc 1 + inc 2 = inc with deltas from inc 1 then inc 2
        case i@ServerStoreIncrementalUpdate(_, _, _) =>
          ServerStoreIncrementalUpdate(baseModelId, deltas ++ i.deltas, i.updatedModelId)
      }
    }
  }

}


