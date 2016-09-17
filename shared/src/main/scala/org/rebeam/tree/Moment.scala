package org.rebeam.tree

/**
  * A moment in time. Used as a light-weight "DateTime" that
  * can be implicitly converted to whatever library is needed.
  * More technically, a moment in the datetime continuum specified
  * as a number of milliseconds from 1970-01-01T00:00Z
  * @param ms number of milliseconds from 1970-01-01T00:00Z
  */
class Moment(val ms: Long) extends AnyVal

