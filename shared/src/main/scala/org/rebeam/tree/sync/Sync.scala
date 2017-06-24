package org.rebeam.tree.sync

import org.rebeam.tree.{Delta, DeltaIOContext}
import io.circe._
import io.circe.syntax._
import io.circe.generic.JsonCodec
import org.rebeam.tree.sync.ServerStoreUpdate.{ServerStoreFullUpdate, ServerStoreIncrementalUpdate}
import cats.syntax.either._
import org.rebeam.tree.DeltaCodecs.DeltaCodec

import scala.util.Try
import scala.util.matching.Regex

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
    * Identifier for a data item, globally unique (globally refers to the whole system under consideration -
    * may just be one server). This uses the id data of the delta where the data item is created, and adds
    * an additional id that makes it unique within that delta.
    * @param clientId       Client id
    * @param clientDeltaId  Id of delta on that client
    * @param id             Id of the item amongst those created in this delta
    * @tparam A             Type of the identified item - Unit if there is no specific identified item.
    */
  case class Guid[A](clientId: ClientId, clientDeltaId: ClientDeltaId, id: Long) {
    override def toString: String = Guid.toString(this)
  }

  object Guid {
    val regex: Regex = "([Gg][Uu][Ii][Dd]-[0-9a-fA-F]+-[0-9a-fA-F]+-[0-9a-fA-F]+)".r
    val regexGrouped: Regex = "[Gg][Uu][Ii][Dd]-([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)".r

    private def hex(x: String): Long = java.lang.Long.parseUnsignedLong(x, 16)

    def fromString[A](s: String): Option[Guid[A]] = s match {
      case regexGrouped(clientId, clientDeltaId, id) =>
        Try {
          Guid[A](ClientId(hex(clientId)), ClientDeltaId(hex(clientDeltaId)), hex(id))
        }.toOption
      case _ => None
    }

    def toString(g: Guid[_]): String = f"guid-${g.clientId.id}%x-${g.clientDeltaId.id}%x-${g.id}%x"

    //Encoder and decoder using plain string format for guid

    implicit def decodeGuid[A]: Decoder[Guid[A]] = Decoder.instance(
      c => c.as[String].flatMap(string => fromString[A](string).fold[Either[DecodingFailure, Guid[A]]](Left(DecodingFailure("Guid invalid string", c.history)))(Right(_)))
    )
    implicit def encodeGuid[A]: Encoder[Guid[A]] = Encoder.instance(
      g => Json.fromString(toString(g))
    )
  }

  /**
    * Indicates a data type has a Guid
    * @tparam A The data type
    */
  trait HasId[A] {
    /**
      * @return The Guid
      */
    def id: Guid[A]
  }

  /**
    * Typeclass for getting guid
    * @tparam A The data type
    */
  trait ToId[A] {
    /**
      * @return The Guid
      */
    def id(a: A): Guid[A]
  }

  implicit def hasIdToId[A <: HasId[A]] = new ToId[A]{ def id(a: A): Guid[A] = a.id }

  /**
    * A reference to a data item with a known Guid.
    * @tparam A The type of data item
    */
  sealed trait Ref[A] {
    /**
      * The Guid of the referenced data item
      * @return Guid
      */
    def guid: Guid[A]

    def optionRevision: Option[Long]
  }

  object Ref {

    /**
      * A reference to data where the specific revision hasn't been specified.
      * This cannot be used to look up data, and will be replaced with a RefResolved
      * in order to enable looking up data at a specific revision.
      * @param guid The guid of the referenced data item
      * @tparam A The type of data item
      */
    case class RefUnresolved[A](guid: Guid[A]) extends Ref[A] {
      lazy val optionRevision: Option[Long] = None

      override def toString: String = Ref.toString(this) + "-u"
    }

    /**
      * A reference to data of a specific revision. Can be used to attempt to look up that
      * data.
      * @param guid The guid of the referenced data item
      * @param revision The revision of the data we are referencing
      * @tparam A The type of data item
      */
    case class RefResolved[A](guid: Guid[A], revision: Long) extends Ref[A]{
      lazy val optionRevision: Option[Long] = Some(revision)
      override def toString: String = Ref.toString(this) + "-rev" + revision
    }

    val regex: Regex = "([Rr][Ee][Ff]-[0-9a-fA-F]+-[0-9a-fA-F]+-[0-9a-fA-F]+)".r
    val regexGrouped: Regex = "[Rr][Ee][Ff]-([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)".r

    private def hex(x: String): Long = java.lang.Long.parseUnsignedLong(x, 16)

    def fromString[A](s: String): Option[Ref[A]] = s match {
      case regexGrouped(clientId, clientDeltaId, id) =>
        Try {
          RefUnresolved[A](Guid[A](ClientId(hex(clientId)), ClientDeltaId(hex(clientDeltaId)), hex(id)))
        }.toOption
      case _ => None
    }

    def toString[A](r: Ref[A]): String = f"ref-${r.guid.clientId.id}%x-${r.guid.clientDeltaId.id}%x-${r.guid.id}%x"

    //Encoder and decoder using plain string format for guid

    implicit def decodeRef[A]: Decoder[Ref[A]] = Decoder.instance(
      c => c.as[String].flatMap(string => fromString[A](string).fold[Either[DecodingFailure, Ref[A]]](Left(DecodingFailure("Ref invalid string", c.history)))(Right(_)))
    )
    implicit def encodeRef[A]: Encoder[Ref[A]] = Encoder.instance(
      r => Json.fromString(toString(r))
    )

    def apply[A](g: Guid[A]): Ref[A] = RefUnresolved(g)
  }

  /**
    * Find HasId instances by their Guid.
    * Works with Cursor.zoomMatch and related methods to zoom to a particular element of a list by id
    * @param id   The id to find
    * @tparam A   The type of item to find, must implement HasId[A]
    */
  @JsonCodec
  case class FindById[A <: HasId[A]](id: Guid[A]) extends (A => Boolean) {
    def apply(a: A): Boolean = a.id == id
  }

  /**
    * A delta and its id
    * @param delta  The Delta
    * @param id     The DeltaId of the Delta
    * @tparam A     The type of model the delta applies to
    */
  case class DeltaAndId[A](delta: Delta[A], id: DeltaId){
    def withContext(context: DeltaIOContext): DeltaWithIC[A] = DeltaWithIC(delta, id, context)
  }

  /**
    * A delta with an id and context
    * @param delta    The delta
    * @param id       The delta's id
    * @param context  The context in which the delta should run
    * @tparam A       The type of model the delta applies to
    */
  case class DeltaWithIC[A](delta: Delta[A], id: DeltaId, context: DeltaIOContext)

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
  case class DeltaWithIJ[A](delta: Delta[A], id: DeltaId, deltaJs: Json) {
    def withContext(context: DeltaIOContext): DeltaWithIJC[A] = DeltaWithIJC(delta, id, deltaJs, context)
  }

  /**
    * A delta with an id and Json encoding, and a DeltaIOContext
    * @param delta    The delta
    * @param id       The delta's id
    * @param deltaJs  The Json encoding of the delta
    * @param context  The context in which the delta should run
    * @tparam A       The type of model the delta applies to
    */
  case class DeltaWithIJC[A](delta: Delta[A], id: DeltaId, deltaJs: Json, context: DeltaIOContext)

  // Client side


  /**
    * ADT for the two options we can receive from the server representing a delta in an update - either just a
    * DeltaId for a delta that came from this client, or a DeltaId and actual delta for a delta
    * generated by another client
    *
    * @tparam A The model type
    */
  sealed trait UpdateDelta[A] {
    def id: DeltaId
  }

  /**
    * A delta generated remotely, by another client, relayed to us by the server in an update
    *
    * @param delta The delta
    * @param id    The id of the delta
    * @param context The context in which to run the delta
    * @tparam A The model type
    */
  case class RemoteDelta[A](delta: Delta[A], id: DeltaId, context: DeltaIOContext) extends UpdateDelta[A]

  /**
    * A delta generated locally, by this client, then referenced by the server in an update
    *
    * @param id The id of the delta
    * @param context The context in which to run the delta
    * @tparam A The model type
    */
  case class LocalDelta[A](id: DeltaId, context: DeltaIOContext) extends UpdateDelta[A]

  /**
    * Sealed trait of model updates sent from server to client
    *
    * @tparam A The model type
    */
  sealed trait ModelUpdate[A]

  /**
    * An incremental update received from the server, building a new model based
    * on an existing base model assumed to be held by the client.
    *
    * @param baseModelId    The ModelId of the base model from which we will produce the updated model
    * @param deltas         The update deltas to apply to the base model to produce the updated model
    * @param updatedModelId The ModelId of the updated model
    * @tparam A The type of model
    */
  case class ModelIncrementalUpdate[A](
                                        baseModelId: ModelId,
                                        deltas: Seq[UpdateDelta[A]],
                                        updatedModelId: ModelId) extends ModelUpdate[A]

  /**
    * A full update received from server, directly setting a new model and overwriting
    * any pending deltas. The first update from the server must always be of this form.
    *
    * @param clientId The client id for the client. Sets id on first update, and
    *                 may be used to change client id on subsequent full updates.
    * @param model    The full model, and id
    * @tparam A The type of model
    */
  case class ModelFullUpdate[A](
                                 clientId: ClientId,
                                 model: ModelAndId[A]) extends ModelUpdate[A]


  def localDeltaDecoder[A]: Decoder[UpdateDelta[A]] =
    Decoder.instance(
      c => {
        val o = c.downField("local")
        for {
          id <- o.downField("id").as[DeltaId]
          context <- o.downField("context").as[DeltaIOContext]
        } yield LocalDelta(id, context)
      }
    )

  def remoteDeltaDecoder[A](implicit dd: Decoder[Delta[A]]): Decoder[UpdateDelta[A]] =
    Decoder.instance(
      c => {
        val o = c.downField("remote")
        for {
          delta <- o.downField("delta").as[Delta[A]]
          id <- o.downField("id").as[DeltaId]
          context <- o.downField("context").as[DeltaIOContext]
        } yield RemoteDelta(delta, id, context)
      }
    )

  def updateDeltaDecoder[A](implicit dd: Decoder[Delta[A]]): Decoder[UpdateDelta[A]] =
    localDeltaDecoder[A] or remoteDeltaDecoder[A]

  def fullUpdateDecoder[A](implicit d: Decoder[A]): Decoder[ModelUpdate[A]] =
    Decoder.instance(c => {
      val o = c.downField("full")
      for {
        clientId <- o.downField("clientId").as[ClientId]
        model <- o.downField("model").as[A]
        id <- o.downField("id").as[ModelId]
      } yield ModelFullUpdate[A](clientId, ModelAndId(model, id))
    })

  def incUpdateDecoder[A](implicit dd: Decoder[Delta[A]]): Decoder[ModelUpdate[A]] = {
    implicit val udd = updateDeltaDecoder[A](dd)
    Decoder.instance(c => {
      val o = c.downField("inc")
      for {
        baseModelId <- o.downField("baseModelId").as[ModelId]
        updatedModelId <- o.downField("updatedModelId").as[ModelId]
        deltas <- o.downField("deltas").as[Vector[UpdateDelta[A]]]
      } yield ModelIncrementalUpdate[A](baseModelId, deltas, updatedModelId)
    })
  }

  def updateDecoder[A](implicit d: Decoder[A], dd: Decoder[Delta[A]]): Decoder[ModelUpdate[A]] =
    fullUpdateDecoder[A] or incUpdateDecoder[A]

  /**
    * Decode incoming messages from the client.
    * Expect incoming messages to be {"commit": {"delta": delta, "id": deltaId}}
    * where delta is Delta[T] to be decoded by deltaDecoder, and deltaId is a DeltaId[T]
    * @param deltaDecoder Decoder for Delta[T]
    * @tparam A           The type of model
    * @return             A decoder of DeltaWithIJ[T]
    */
  def clientMsgDecoder[A](implicit deltaDecoder: DeltaCodec[A]): Decoder[DeltaWithIJ[A]] = Decoder.instance(c => {

    val o = c.downField("commit")

    // We want to try to get the actual Json in the delta field value, this
    // leads to some rather convoluted code
    val d: Decoder.Result[Json] = o.downField("delta").focus
      .map(Right[DecodingFailure, Json])
      .getOrElse(Left[DecodingFailure, Json](DecodingFailure("Expected a delta field in commit object", o.history)))

    for {
      delta <- o.downField("delta").as[Delta[A]]
      id <- o.downField("id").as[DeltaId]
      deltaJs <- d
    } yield DeltaWithIJ(delta, id, deltaJs)
  })

  def clientMsgEncoder[A]: Encoder[DeltaWithIJ[A]] = Encoder.instance{
    dij => Json.obj(
      "commit" -> Json.obj(
        "delta" -> dij.deltaJs,
        "id" -> dij.id.asJson
      )
    )
  }


  //Server side

  def serverStoreUpdateEncoder[A](clientId: ClientId)(implicit encoder: Encoder[A]): Encoder[ServerStoreUpdate[A]] = Encoder.instance {
    case ServerStoreFullUpdate(modelAndId) =>
      Json.obj(
        "full" -> Json.obj(
          "clientId" -> clientId.asJson,
          "model" -> encoder(modelAndId.model),
          "id" -> modelAndId.id.asJson
        )
      )

    case ServerStoreIncrementalUpdate(baseModelId, deltas, updatedModelId) =>

      // Convert deltas to Json, "compressing" by encoding local deltas
      // that were generated by this client by omitting the encoded delta itself.
      // All deltas have an id and context though.
      val deltasJs = deltas.map(
        d => if (d.id.clientId == clientId) {
          Json.obj(
            "local" -> Json.obj(
              "id" -> d.id.asJson,
              "context" -> d.context.asJson
            )
          )
        } else {
          Json.obj(
            "remote" -> Json.obj(
              "delta" -> d.deltaJs,
              "id" -> d.id.asJson,
              "context" -> d.context.asJson
            )
          )
        }
      )

      Json.obj(
        "inc" -> Json.obj(
          "baseModelId" -> baseModelId.asJson,
          "updatedModelId" -> updatedModelId.asJson,
          "deltas" -> Json.arr(deltasJs: _*)
        )
      )
  }


}
