package org.rebeam.tree.sync

import org.rebeam.tree.sync.Sync.{ClientDeltaId, ClientId}

import scala.util.Try
import scala.util.matching.Regex
import io.circe._

/**
  * A reference to a data item with a known Id.
  * @tparam A The type of data item
  */
case class Ref[+A](id: Id[A]) extends Identified[A] {
  override def toString: String = Ref.toString(this)
}

object Ref {
  val regex: Regex = "([Rr][Ee][Ff]-[0-9a-fA-F]+-[0-9a-fA-F]+-[0-9a-fA-F]+)".r
  val regexGrouped: Regex = "[Rr][Ee][Ff]-([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)".r

  private def hex(x: String): Long = java.lang.Long.parseUnsignedLong(x, 16)

  def fromString[A](s: String): Option[Ref[A]] = s match {
    // FIXME also encode the revision?
    case regexGrouped(clientId, clientDeltaId, id) =>
      Try {
        Ref[A](Id[A](Guid(ClientId(hex(clientId)), ClientDeltaId(hex(clientDeltaId)), WithinDeltaId(hex(id)))))
      }.toOption
    case _ => None
  }

  def toString[A](r: Ref[A]): String = f"ref-${r.id.guid.clientId.id}%x-${r.id.guid.clientDeltaId.id}%x-${r.id.guid.withinDeltaId.id}%x"

  //Encoder and decoder using plain string format for guid

  implicit def decodeRef[A]: Decoder[Ref[A]] = Decoder.instance(
    c => c.as[String].flatMap(string => fromString[A](string).fold[Either[DecodingFailure, Ref[A]]](Left(DecodingFailure("Ref invalid string", c.history)))(Right(_)))
  )
  implicit def encodeRef[A]: Encoder[Ref[A]] = Encoder.instance(
    r => Json.fromString(toString(r))
  )
}
