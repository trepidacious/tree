package org.rebeam.tree.random

/**
  * Pure Pseudorandom number generator, operates like State
  */
trait PRandom {
  def int: (PRandom, Int)
  def intUntil(bound: Int): (PRandom, Int)
  def long: (PRandom, Long)
  def boolean: (PRandom, Boolean)
  def float: (PRandom, Float)
  def double: (PRandom, Double)
}

object PRandom {
  /**
    * Creates a new PRandom from a seed.
    * The PRandom will have the same behaviour as java.util.Random.
    *
    * @param seed the initial seed
    */
  def apply(seed: Long): PRandom = PRandomDefault.from(seed)
}

/**
  * Default PRandom, operates like java.util.Random
  * @param seed The internal seed - do NOT initialise using case class constructor, use PRandomDefault.from
  */
private[this] case class PRandomDefault(seed: Long) extends PRandom {

  import PRandomDefault._

  private[PRandomDefault] def next(bits: Int): (PRandomDefault, Int) = {
    val nextSeed = (seed * multiplier + addend) & mask
    (PRandomDefault(nextSeed), (nextSeed >>> (48 - bits)).toInt)
  }

  def int: (PRandom, Int) = next(32)

  def intUntil(bound: Int): (PRandom, Int) = {
    require(bound > 0, "bound must be positive")

    val m = bound - 1

    // If bound is power of 2
    if ((bound & m) == 0) {
      val (p, r) = next(31)
      (p, ((bound * r.toLong) >> 31).toInt)

    } else {

      val (x, y) = next(31)

      var p = x
      var u = y

      var r = u % bound
      while (u - r + m < 0) {
        val (np, nu) = p.next(31)
        p = np
        u = nu
        r = u % bound
      }
      (p, r)
    }
  }

  def long: (PRandom, Long) = {
    val (p2, x) = next(32)
    val (p3, y) = p2.next(32)

    // it's okay that the bottom word remains signed.
    (p3, (x.toLong << 32) + y)
  }

  def boolean: (PRandom, Boolean) = {
    val (p, r) = next(1)
    (p, r != 0)
  }

  def float: (PRandom, Float) = {
    val (p, r) = next(24)
    (p, r / (1 << 24).toFloat)
  }

  def double: (PRandom, Double) = {
    val (p2, x) = next(26)
    val (p3, y) = p2.next(27)
    (p3, ((x.toLong << 27) + y) * doubleUnit)
  }

}

private object PRandomDefault {
  val multiplier: Long = 0x5DEECE66DL
  val addend: Long = 0xBL
  val mask: Long = (1L << 48) - 1
  val doubleUnit: Double = 1.0d / (1L << 53).toDouble

  def from(seed: Long): PRandom = PRandomDefault(initialScramble(seed))
  private def initialScramble(seed: Long): Long = (seed ^ multiplier) & mask
}
