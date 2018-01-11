package org.rebeam.tree

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._
import monocle._
import monocle.macros.Lenses

object DeltaCodecProto {

  trait Delta[A] {
    def apply(a: A): A
  }

  case class LensDelta[A, B](lens: Lens[A, B], d: Delta[B]) extends Delta[A] {
    def apply(a: A): A = lens.modify(d.apply)(a)
  }

  case class ValueDelta[A](v: A) extends Delta[A] {
    def apply(a: A): A = v
  }

  trait PartialDeltaCodec[A] {
    def encode(a: Delta[A]): Option[Json]
    val decoder: Decoder[Delta[A]]

    def or(other: PartialDeltaCodec[A]): PartialDeltaCodec[A] =
      EitherPartialDeltaCodec(this, other)
  }

  case class EitherPartialDeltaCodec[A](lc: PartialDeltaCodec[A], rc: PartialDeltaCodec[A]) extends PartialDeltaCodec[A] {
    def encode(a: Delta[A]): Option[Json] = lc.encode(a).orElse(rc.encode(a))
    val decoder: Decoder[Delta[A]] = lc.decoder or rc.decoder
  }

  case class PartialLensDeltaCodec[A, B]
  (name: String, lens: Lens[A, B])
  (implicit
   partialBCodec: PartialDeltaCodec[B]
  ) extends PartialDeltaCodec[A] {

    implicit val deltaBDecoder: Decoder[Delta[B]] = partialBCodec.decoder

    def encode(a: Delta[A]): Option[Json] = a match {
      case LensDelta(l, d) if l == lens =>
        partialBCodec.encode(d.asInstanceOf[Delta[B]])
          .map(dJson =>
            Json.obj(
              "LensDelta" -> Json.obj(
                name -> dJson
              )
            )
          )
      case _ => None
    }

    val decoder: Decoder[Delta[A]] = Decoder.instance {
      c => c.downField("LensDelta").downField(name).as[Delta[B]]
        .map(db => LensDelta(lens, db))
    }
  }

  def partialValueDeltaCodec[A](
                                 implicit
                                 deltaBEncoder: Encoder[A],
                                 deltaBDecoder: Decoder[A]
                               ): PartialDeltaCodec[A] = new PartialDeltaCodec[A] {

    def encode(a: Delta[A]): Option[Json] = a match {
      case ValueDelta(v) =>
        Some(Json.obj(
          "ValueDelta" -> v.asJson
        ))
      case _ => None
    }

    val decoder: Decoder[Delta[A]] = Decoder.instance {
      c => c.downField("ValueDelta").as[A]
        .map(v => ValueDelta(v))
    }
  }

  implicit val pString: PartialDeltaCodec[String] = partialValueDeltaCodec
  implicit val pInt: PartialDeltaCodec[Int] = partialValueDeltaCodec

  // Demo Data

  @Lenses
  case class Person(name: String, age: Int)
  object Person {
    implicit val encoder: Encoder[Person] = deriveEncoder
    implicit val decoder: Decoder[Person] = deriveDecoder
  }

  implicit val partialPersonDeltaCodec: PartialDeltaCodec[Person] =
    partialValueDeltaCodec[Person] or PartialLensDeltaCodec("name", Person.name) or PartialLensDeltaCodec("age", Person.age)

  def main(args: Array[String]): Unit = {

    val nameDelta: Delta[Person] = LensDelta(Person.name, ValueDelta("bob"))
    val ageDelta: Delta[Person] = LensDelta(Person.age, ValueDelta(20))

    println(partialPersonDeltaCodec.encode(nameDelta))
    println(partialPersonDeltaCodec.encode(ageDelta))

  }

}
