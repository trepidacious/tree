package org.rebeam.tree.sync

import org.rebeam.tree.sync.Sync._

sealed trait ServerStoreUpdate[A]

object ServerStoreUpdate {
  /**
    * Generated to set a complete new model. The first update to an observer
    * will always be a ServerStoreFullUpdate
    * @param modelAndId Model and id
    */
  case class ServerStoreFullUpdate[A](modelAndId: ModelAndId[A]) extends ServerStoreUpdate[A]

  /**
    * Called each time the ServerStore's model changes due to one or more
    * deltas
    * @param baseModelId      The id of the model before the updates
    * @param updates          The deltas that have been applied, in the order
    *                         they were applied
    * @param updatedModelId   The id of the model after the updates
    */
  case class ServerStoreIncrementalUpdate[A](baseModelId: ModelId,
                                             updates: Seq[DeltaWithIJ[A]],
                                             updatedModelId: ModelId) extends ServerStoreUpdate[A]

}


