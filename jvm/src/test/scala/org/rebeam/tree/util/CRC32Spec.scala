package org.rebeam.tree.util

import org.scalacheck.Arbitrary._
import org.scalatest._
import org.scalatest.prop.Checkers

import java.util.zip.{CRC32 => J32}

class CRC32Spec extends WordSpec with Matchers with Checkers {

  "CRC32" should {
    "generate the same correct result as java.util.zip.CRC32 for example string" in {

      val data = "The quick brown fox jumps over the lazy dog".getBytes

      val j32 = new J32
      j32.update(data)

      val crc32 = CRC32(data)

      // Known result
      assert(j32.getValue.toHexString == "414fa339")

      // Check crc32 matches
      assert(crc32.value == j32.getValue.toInt)
    }

    "generate the same result as java.util.zip.CRC32 for arbitrary byte sequences" in {
      check((data: Array[Byte]) => {
        val j32 = new J32
        j32.update(data)

        val crc32 = CRC32(data)

        // Check crc32 matches java version
        (crc32.value == j32.getValue.toInt) && (crc32.hex == j32.getValue.toHexString)

      }, MinSuccessful(10000))
    }

  }

}
