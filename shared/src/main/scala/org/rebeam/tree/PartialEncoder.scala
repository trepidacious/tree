package org.rebeam.tree

import io.circe.{Encoder, Json}

/**
  * An Encoder that can only encode some input values, and so produces Option[Json]
  *
  * @tparam A The type of possibly-encodable values
  */
trait PartialEncoder[A] extends Serializable { self =>
  /**
    * Convert a value to Some(JSON) if possible, None if not.
    */
  def apply(a: A): Option[Json]

  /**
    * Create a new [[PartialEncoder]] by applying a function to a value of type `B` before encoding as an
    * `A`.
    */
  final def contramap[B](f: B => A): PartialEncoder[B] = a => self(f(a))

  /**
    * Create a new [[PartialEncoder]] by applying a function to the output of this one.
    */
  final def mapJson(f: Json => Json): PartialEncoder[A] = a => self(a).map(f)

  final def or(o: PartialEncoder[A]): PartialEncoder[A] = a => self(a).orElse(o(a))
}

object PartialEncoder {
  def apply[A](e: Encoder[A]): PartialEncoder[A] = a => Some(e(a))
}