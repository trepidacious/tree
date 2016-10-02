package org.rebeam.tree

import io.circe.Json

/**
  * Encode an instance around another already-encoded instance.
  * This is particularly useful for nested lenses being encoded
  * from the bottom up, in a Parent.
  */
trait OuterEncoder[A] {
  def encode(a: A, deltaJs: Json): Json
}

object OuterEncoder {
  def instance[A](f: (A, Json) => Json): OuterEncoder[A] = new OuterEncoder[A] {
    override def encode(a: A, deltaJs: Json): Json = f(a, deltaJs)
  }
}
