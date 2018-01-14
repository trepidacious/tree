package org.rebeam.tree.util

/**
  * A CRC32, can be updated and read
  */
trait CRC32 extends Any {
  /**
    * Update the CRC32 with a byte, stored as an Int
    * @param byte The byte, as an Int
    * @return     The new CRC32
    */
  def updateByte(byte: Int): CRC32

  /**
    * Update the CRC32 with an Int, using bytes in
    * big-endian order
    * @param i  The Int
    * @return   The new CRC32
    */
  def updateInt(i: Int): CRC32

  /**
    * Update the CRC32 with a Long, using bytes in
    * big-endian order
    * @param l  The Long
    * @return   The new CRC32
    */
  def updateLong(l: Long): CRC32

  /**
    * The value of the CRC32, note this is the 1's complement of
    * the hash value. This is just convention, allowing for faster
    * comparison of hashes?
    * @return The CRC32
    */
  def value: Int

  /**
    * Value as hex
    * @return hex
    */
  def hex: String = value.toHexString

}

object CRC32 {

  private case class H(hash: Int) extends AnyVal with CRC32 {

    def updateByte(byte: Int): CRC32 = H(CRC32.table((hash ^ byte) & 255) ^ (hash >>> 8))

    def updateInt(i: Int): CRC32 = {
      updateByte((i>>24) & 0xFF)
        .updateByte((i>>16) & 0xFF)
        .updateByte((i>>8) & 0xFF)
        .updateByte(i & 0xFF)
    }

    def updateLong(l: Long): CRC32 = {
      updateByte((l>>60).toInt & 0xFF)
        .updateByte((l>>56).toInt & 0xFF)
        .updateByte((l>>48).toInt & 0xFF)
        .updateByte((l>>40).toInt & 0xFF)
        .updateByte((l>>32).toInt & 0xFF)
        .updateByte((l>>24).toInt & 0xFF)
        .updateByte((l>>16).toInt & 0xFF)
        .updateByte((l>>8).toInt & 0xFF)
        .updateByte(l.toInt & 0xFF)
    }

    def value: Int = ~hash
  }

  val empty: CRC32 = H(-1)

  /**
    * Produce the CRC32 of a sequence of bytes
    * @param bytes  The bytes
    * @return       The CRC32
    */
  def apply(bytes: Seq[Byte]): CRC32 = bytes.foldLeft(CRC32.empty){case (c, b) => c.updateByte(b.intValue())}

  private lazy val table: Array[Int] = Array.tabulate(256) { n =>
    var c = n
    var k = 7

    while (k >= 0) {
      c = if ((c & 1) != 0) {
        (c >>> 1) ^ -306674912
      } else {
        c >>> 1
      }

      k -= 1
    }

    c
  }
}
