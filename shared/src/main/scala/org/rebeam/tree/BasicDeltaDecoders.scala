package org.rebeam.tree

object BasicDeltaDecoders {
  import DeltaCodecs._
  implicit val stringDeltaReader: DeltaCodec[String] = value[String]
  implicit val booleanDeltaReader: DeltaCodec[Boolean] = value[Boolean]
  implicit val byteDeltaReader: DeltaCodec[Byte] = value[Byte]
  implicit val shortDeltaReader: DeltaCodec[Short] = value[Short]
  implicit val intDeltaReader: DeltaCodec[Int] = value[Int]
  implicit val longDeltaReader: DeltaCodec[Long] = value[Long]
  implicit val floatDeltaReader: DeltaCodec[Float] = value[Float]
  implicit val doubleDeltaReader: DeltaCodec[Double] = value[Double]
}
