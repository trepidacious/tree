package org.rebeam.tree.ref

import org.rebeam.tree.sync.Sync.{ClientDeltaId, ClientId, Guid}

import scala.util.Try
import scala.util.matching.Regex

import io.circe._
import cats.syntax.either._


/**
  * A reference to a data item with a known Guid.
  *
  * @tparam A The type of data item
  */
sealed trait Ref[+A] {
  /**
    * The Guid of the referenced data item
    * @return Guid
    */
  def guid: Guid[A]

  /**
    * The revision of the referenced data item, if known.
    * We use an additional Guid, which is generated using the
    * delta that produces the revision. This ensures that whenever
    * the data changes, we have a new, unique revision.
    * @return Revision
    */
  def optionRevision: Option[Guid[A]]
}

object Ref {

  /**
    * A reference to data where the specific revision hasn't been specified.
    * Will be replaced by a RefResolved when the cache contains the referenced
    * data.
    * As for all Refs, will look up the most recent revision of data in a Cache.
    * @param guid The guid of the referenced data item
    * @tparam A The type of data item
    */
  case class RefUnresolved[A](guid: Guid[A]) extends Ref[A] {
    lazy val optionRevision: Option[Guid[A]] = None

    override def toString: String = Ref.toString(this) + "-u"
  }

  /**
    * A reference to data of a specific revision. Can be used to attempt to look up that
    * data.
    * As for all Refs, will look up the most recent revision of data in a Cache.
    * @param guid The guid of the referenced data item
    * @param revision The revision of the data we are referencing
    * @tparam A The type of data item
    */
  case class RefResolved[A](guid: Guid[A], revision: Guid[A]) extends Ref[A]{
    lazy val optionRevision: Option[Guid[A]] = Some(revision)
    override def toString: String = Ref.toString(this) + "-rev" + revision
  }

  val regex: Regex = "([Rr][Ee][Ff]-[0-9a-fA-F]+-[0-9a-fA-F]+-[0-9a-fA-F]+)".r
  val regexGrouped: Regex = "[Rr][Ee][Ff]-([0-9a-fA-F]+)-([0-9a-fA-F]+)-([0-9a-fA-F]+)".r

  private def hex(x: String): Long = java.lang.Long.parseUnsignedLong(x, 16)

  def fromString[A](s: String): Option[Ref[A]] = s match {
    // FIXME also encode the revision?
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
