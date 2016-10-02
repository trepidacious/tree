package org.rebeam.tree

import io.circe._

trait Codec[A] extends Decoder[A] with Encoder[A]

object Codec {
  def apply[A](encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = PairCodec(encoder, decoder)

  case class PairCodec[A](encoder: Encoder[A], decoder: Decoder[A]) extends Codec[A] {
    /**
      * Convert a value to JSON.
      */
    def apply(a: A): Json = encoder(a)

    /**
      * Decode the given hcursor.
      */
    def apply(c: HCursor): Decoder.Result[A] = decoder(c)
  }
}
