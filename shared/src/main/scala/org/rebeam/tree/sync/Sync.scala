package org.rebeam.tree.sync

import io.circe._
import io.circe.generic.JsonCodec
import org.rebeam.tree.Delta

object Sync {

  /**
    * Globally unique identifier for a client (globally refers to the whole system under consideration -
    * may just be one server).
    * Assigned by server.
    *
    * @param id The identifier value
    */
  @JsonCodec
  case class ClientId(id: Long) extends AnyVal

  /**
    * Identifier for a delta, unique for a given client but not globally
    * Assigned by each client for deltas it generates.
    *
    * @param id The identifier value
    */
  @JsonCodec
  case class ClientDeltaId(id: Long) extends AnyVal {
    def next: ClientDeltaId = ClientDeltaId(id + 1)
  }

  /**
    * Identifier for a delta, globally unique (globally refers to the whole system under consideration -
    * may just be one server).
    *
    * @param clientId Client id
    * @param clientDeltaId Id of delta on that client
    */
  @JsonCodec
  case class DeltaId(clientId: ClientId, clientDeltaId: ClientDeltaId)

  /**
    * A delta and its id
    * @param delta  The Delta
    * @param id     The DeltaId of the Delta
    * @tparam A     The type of model the delta applies to
    */
  case class DeltaAndId[A](delta: Delta[A], id: DeltaId)

  /**
    * Globally likely-unique identifier for a model revision. This may be an actual guid, or a good
    * quality hash.
    *
    * @param id Identifier
    */
  @JsonCodec
  case class ModelId(id: Long) extends AnyVal

  /**
    * A model and its id
    * @param model  The model
    * @param id     The ModelId of the model
    * @tparam A     The type of the model
    */
  case class ModelAndId[A](model: A, id: ModelId)

  /**
    * Typeclass to generate a ModelId from a model. May provide
    * None if ModelId cannot be recreated from the model - e.g. if it
    * is not a hash but an incrementing index.
    * @tparam A     The type of the model
    */
  trait ModelIdGen[A] {
    def genId(model: A): Option[ModelId]
  }

  /**
    * A delta with an id and Json encoding
    * @param delta    The delta
    * @param id       The delta's id
    * @param deltaJs  The Json encoding of the delta
    * @tparam A       The type of model the delta applies to
    */
  case class DeltaWithIJ[A](delta: Delta[A], id: DeltaId, deltaJs: Json)

}
