package org.rebeam.tree

import io.circe.generic.JsonCodec

/**
  * A moment in time. Used as a light-weight "DateTime" that
  * can be implicitly converted to whichever library is needed
  * on the server or client.
  * This is as a number of milliseconds from 1970-01-01T00:00UTC±00:00,
  * i.e. midnight, January 1, 1970 UTC. This therefore includes any
  * leap seconds adjustment to UTC.
  * This ignores timezones - it is left to "DateTime" libraries to
  * format this with a timezone, factoring in all the nonsense that
  * involves.
  * Practically, this is provided by System.currentTimeMillis() on JVM
  * and ScalaJS, and generally produced by running a DeltaIO.
 *
  * @param ms number of milliseconds from 1970-01-01T00:00UTC±00:00
  */
@JsonCodec
case class Moment(ms: Long) extends AnyVal

