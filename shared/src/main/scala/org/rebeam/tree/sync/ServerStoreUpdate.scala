package org.rebeam.tree.sync

import org.rebeam.tree.sync.Sync._
import DeltaIORun._

sealed trait ServerStoreUpdate[A] {
  def append(e: ServerStoreUpdate[A])(implicit refAdder: RefAdder[A]): ServerStoreUpdate[A]
}

object ServerStoreUpdate {
  /**
    * Generated to set a complete new model. The first update to an observer
    * will always be a ServerStoreFullUpdate
    * @param modelAndId Model and id
    */
  case class ServerStoreFullUpdate[A](modelAndId: ModelAndId[A]) extends ServerStoreUpdate[A] {
    def append(e: ServerStoreUpdate[A])(implicit refAdder: RefAdder[A]): ServerStoreUpdate[A] = {
      e match {
        // Full 1 + full 2 = full 2
        case f@ServerStoreFullUpdate(_) => f

        // Full 1 + inc 1 = new full update with model from full 1 update with deltas from inc 1
        case i@ServerStoreIncrementalUpdate(_, _, _) =>
          val updatedModel = i.deltas.foldLeft(modelAndId.model){
            case (m, dijc) => {
              val result = dijc.runWith(m)
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
  case class ServerStoreIncrementalUpdate[A](baseModelId: ModelId,
                                             deltas: Seq[DeltaWithIJC[A]],
                                             updatedModelId: ModelId) extends ServerStoreUpdate[A] {
    def append(e: ServerStoreUpdate[A])(implicit refAdder: RefAdder[A]): ServerStoreUpdate[A] = {
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


