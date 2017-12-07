package org.rebeam.tree.sync

import org.rebeam.tree.sync.Sync._
import io.circe._
import io.circe.generic.JsonCodec
import cats.syntax.either._

import scala.util.Try
import scala.util.matching.Regex

/**    
  * Last part of the identifier for a Guid, unique for a given client delta but not globally.
  * Assigned by each client within deltas it generates.
  *
  * @param id The identifier value
  */
@JsonCodec
case class WithinDeltaId(id: Long) extends AnyVal

/**
  * Globally unique identifier (globally refers to the whole system under consideration -
  * may just be one server). This uses the id data of the delta where the data item is created, and adds
  * an additional id that makes it unique within that delta.
  * This is a generic identifier without an association with an item of data, or type of data, and as
  * such is just a unique token. Use IdOf to associate to a particular data type.
  * @param clientId       Client id
  * @param clientDeltaId  Id of delta on that client
  * @param withinDeltaId  Id of the item amongst those created in this delta
  */
case class Guid(clientId: ClientId, clientDeltaId: ClientDeltaId, withinDeltaId: WithinDeltaId) {
  override def toString: String = Guid.toString(this)
}

object Guid {
  val regex: Regex = "([Gg][Uu][Ii][Dd]-[0-9a-fA-F]+-[0-9a-fA-F]+-[0-9a-fA-F]+)".r
  val regexGrouped: Regex = "[Gg][Uu][Ii][Dd]-([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)".r

  private def hex(x: String): Long = java.lang.Long.parseUnsignedLong(x, 16)

  def fromString(s: String): Option[Guid] = s match {
    case regexGrouped(clientId, clientDeltaId, id) =>
      Try {
        Guid(ClientId(hex(clientId)), ClientDeltaId(hex(clientDeltaId)), WithinDeltaId(hex(id)))
      }.toOption
    case _ => None
  }

  def toString(g: Guid): String = f"guid-${g.clientId.id}%x-${g.clientDeltaId.id}%x-${g.withinDeltaId.id}%x"

  //Encoder and decoder using plain string format for guid

  implicit val decodeGuid: Decoder[Guid] = Decoder.instance(
    c => c.as[String].flatMap(string => fromString(string).fold[Either[DecodingFailure, Guid]](Left(DecodingFailure("Guid invalid string", c.history)))(Right(_)))
  )
  implicit val encodeGuid: Encoder[Guid] = Encoder.instance(
    g => Json.fromString(toString(g))
  )

  implicit val guidKeyEncoder: KeyEncoder[Guid] = new KeyEncoder[Guid] {
    override def apply(key: Guid): String = Guid.toString(key)
  }

  implicit val guidKeyDecoder: KeyDecoder[Guid] = new KeyDecoder[Guid] {
    override def apply(key: String): Option[Guid] = Guid.fromString(key)
  }

}

/**
  * An identifier for an item of data of a known type, using a Guid 
  * @param guid The Guid
  * @tparam A Type of the identified item
  */
case class Id[+A](guid: Guid)

object Id {
  val regex: Regex = "([Ii][Dd]-[0-9a-fA-F]+-[0-9a-fA-F]+-[0-9a-fA-F]+)".r
  val regexGrouped: Regex = "[Ii][Dd]-([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)".r

  private def hex(x: String): Long = java.lang.Long.parseUnsignedLong(x, 16)

  def fromString[A](s: String): Option[Id[A]] = s match {
    // FIXME also encode the revision?
    case regexGrouped(clientId, clientDeltaId, id) =>
      Try {
        Id[A](Guid(ClientId(hex(clientId)), ClientDeltaId(hex(clientDeltaId)), WithinDeltaId(hex(id))))
      }.toOption
    case _ => None
  }


  def toString[A](r: Id[A]): String = f"id-${r.guid.clientId.id}%x-${r.guid.clientDeltaId.id}%x-${r.guid.withinDeltaId.id}%x"

  //Encoder and decoder using plain string format for id
  implicit def decodeId[A]: Decoder[Id[A]] = Decoder.instance(
    c => c.as[String].flatMap(string => fromString[A](string).fold[Either[DecodingFailure, Id[A]]](Left(DecodingFailure("Ref invalid string", c.history)))(Right(_)))
  )
  implicit def encodeId[A]: Encoder[Id[A]] = Encoder.instance(
    r => Json.fromString(toString(r))
  )

  implicit def idKeyEncoder[A]: KeyEncoder[Id[A]] = new KeyEncoder[Id[A]] {
    override def apply(id: Id[A]): String = Id.toString(id)
  }

  implicit def idKeyDecoder[A]: KeyDecoder[Id[A]] = new KeyDecoder[Id[A]] {
    override def apply(key: String): Option[Id[A]] = Id.fromString(key)
  }

}

/**
  * Indicates a data type has an Id
  * @tparam A Type of the identified item
  */
trait Identified[+A] {
  /**
    * @return The id
    */
  def id: Id[A]
}

/**
  * Typeclass for getting an Id from a data item
  * @tparam A Type of the identified item
  */
trait Identifiable[A] {
  /**
    * @return The Guid
    */
  def id(a: A): Id[A]
}

object Identifiable {
  /**
    * Identified data is trivially identifiable
    */
  implicit def identified2Identifiable[A <: Identified[A]] = new Identifiable[A]{ def id(a: A): Id[A] = a.id }  
} 
