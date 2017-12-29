package org.rebeam.tree.util

import io.circe._
import io.circe.syntax._

object CirceUtils {
  def enc[A: Encoder](typeName: String, a: A): Json = Json.obj("type" -> typeName.asJson, "value" -> a.asJson)
  def dec[A](f: PartialFunction[String, Decoder[_ <: A]]): Decoder[A] =
    Decoder[String].prepare(_.downField("type")).flatMap(
      t => f.lift(t).getOrElse(Decoder.failedWithMessage(s"invalid type: $t")).map(a => a: A)
    )
}
