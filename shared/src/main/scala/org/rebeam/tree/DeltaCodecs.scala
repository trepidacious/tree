package org.rebeam.tree

import io.circe._
import io.circe.syntax._
import monocle.{Lens, Optional, Prism}
import org.rebeam.tree.ref.{Mirror, MirrorCodec}
import org.rebeam.tree.sync._

import scala.reflect.ClassTag

object DeltaCodecs {

  trait Codec[A] {
    def encoder: PartialEncoder[A]
    val decoder: Decoder[A]
    def or(other: => Codec[A]): Codec[A] =
      EitherCodec(this, other)
  }

  case class EitherCodec[A](lc: Codec[A], rc: Codec[A]) extends Codec[A] {
    val encoder: PartialEncoder[A] = lc.encoder or rc.encoder
    val decoder: Decoder[A] = lc.decoder or rc.decoder
  }

  type DeltaCodec[A] = Codec[Delta[A]]

  case class EitherPartialDeltaCodec[A](lc: DeltaCodec[A], rc: DeltaCodec[A]) extends DeltaCodec[A] {
    val encoder: PartialEncoder[Delta[A]] = lc.encoder or rc.encoder
    val decoder: Decoder[Delta[A]] = lc.decoder or rc.decoder
  }

  def lens[A, B]
    (name: String, lens: Lens[A, B])
    (implicit partialBCodec: DeltaCodec[ B]): DeltaCodec[A] = new DeltaCodec[A] {

    val encoder: PartialEncoder[Delta[A]] = {
      case LensDelta(l, d) if l == lens =>
        //Note we know that delta is a Delta[B] since LensDelta has the same lens used to
        //create this instance, which was Lens[A, B]
        partialBCodec.encoder(d.asInstanceOf[Delta[B]])
          .map(dJson =>
            Json.obj(
              "LensDelta" -> Json.obj(
                name -> dJson
              )
            )
          )
      case x => None
    }

    val decoder: Decoder[Delta[A]] = Decoder.instance {
      c => c.downField("LensDelta")
            .downField(name)
            .as[Delta[B]](partialBCodec.decoder)
            .map(db => LensDelta(lens, db))
    }
  }

  def optional[A, B]
  (name: String, optional: Optional[A, B])
  (implicit partialBCodec: DeltaCodec[B]): DeltaCodec[A] = new DeltaCodec[A] {

    val encoder: PartialEncoder[Delta[A]] = {
      case OptionalDelta(o, d) if o == optional =>
        partialBCodec.encoder(d.asInstanceOf[Delta[B]])
          .map(dJson =>
            Json.obj(
              "OptionalDelta" -> Json.obj(
                name -> dJson
              )
            )
          )
      case _ => None
    }

    val decoder: Decoder[Delta[A]] = Decoder.instance {
      c => c.downField("OptionalDelta")
        .downField(name)
        .as[Delta[B]](partialBCodec.decoder)
        .map(db => OptionalDelta(optional, db))
    }
  }

  def value[A](
    implicit
    deltaBEncoder: Encoder[A],
    deltaBDecoder: Decoder[A]
  ): DeltaCodec[A] = new DeltaCodec[A] {

    val encoder: PartialEncoder[Delta[A]] = {
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

  def optionalI[A](implicit dCodecA: DeltaCodec[A]): DeltaCodec[List[A]] = new DeltaCodec[List[A]] {
    val encoder: PartialEncoder[Delta[List[A]]] = {
      case OptionalIDelta(optionalI, delta) =>
        //Note we know that delta is a Delta[A] since OptionalIDelta is a Delta[List[A]]
        dCodecA.encoder(delta.asInstanceOf[Delta[A]]).map(deltaJson =>
          Json.obj(
            "OptionalIDelta" -> Json.obj(
              "index" -> optionalI.index.asJson,
              "delta" -> deltaJson
            )
          )
        )
      case _ => None
    }

    val decoder: Decoder[Delta[List[A]]] = Decoder.instance { c =>
      val o = c.downField("OptionalIDelta")
      for {
        index <- o.downField("index").as[Int]
        delta <- o.downField("delta").as[Delta[A]](dCodecA.decoder)
      } yield OptionalIDelta[A](OptionalI(index), delta)
    }
  }

  def optionalId[A <: Identified[A]]
  (implicit dCodecA: DeltaCodec[A]): DeltaCodec[List[A]] = new DeltaCodec[List[A]] {
    val encoder: PartialEncoder[Delta[List[A]]] = {
      case OptionalIdDelta(optionalId, delta) =>
        for {
          deltaJson <- dCodecA.encoder(delta.asInstanceOf[Delta[A]])
        } yield {
          Json.obj(
            "OptionalIdDelta" -> Json.obj(
              "id" -> optionalId.id.asJson,
              "delta" -> deltaJson
            )
          )
        }
      case _ => None
    }

    val decoder: Decoder[Delta[List[A]]] = Decoder.instance { c =>
      val o = c.downField("OptionalIdDelta")
      for {
        id <- o.downField("id").as[Id[A]]
        delta <- o.downField("delta").as[Delta[A]](dCodecA.decoder)
      } yield OptionalIdDelta[A](OptionalId(id), delta)
    }
  }

  def optionalMatch[A, F <: A => Boolean]
    (name: String)
    (implicit dCodecA: DeltaCodec[A], fEncoder: Encoder[F], fDecoder: Decoder[F], ctF: ClassTag[F]): DeltaCodec[List[A]] = new DeltaCodec[List[A]] {
      val encoder: PartialEncoder[Delta[List[A]]] = {
        case OptionalMatchDelta(optionalMatch, delta) =>
          for {
            deltaJson <- dCodecA.encoder(delta.asInstanceOf[Delta[A]])
            f <- ctF.unapply(optionalMatch.f)
          } yield {
            Json.obj(
              "OptionalMatchDelta" -> Json.obj(
                name -> Json.obj(
                  "find" -> f.asJson,
                  "delta" -> deltaJson
                )
              )
            )
          }
        case _ => None
      }

      val decoder: Decoder[Delta[List[A]]] = Decoder.instance { c =>
        val o = c.downField("OptionalMatchDelta").downField(name)
        for {
          f <- o.downField("find").as[F]
          delta <- o.downField("delta").as[Delta[A]](dCodecA.decoder)
        } yield OptionalMatchDelta[A, F](OptionalMatch(f), delta)
      }
    }

  def option[A](implicit dCodecA: DeltaCodec[A]): DeltaCodec[Option[A]] = new DeltaCodec[Option[A]] {
    val encoder: PartialEncoder[Delta[Option[A]]] = {
      case OptionDelta(delta) =>
        //Note we know that delta is a Delta[A] since OptionDelta is a Delta[Option[A]]
        dCodecA.encoder(delta.asInstanceOf[Delta[A]]).map(deltaJson =>
          Json.obj(
            "OptionDelta" -> Json.obj(
              "delta" -> deltaJson
            )
          )
        )
      case _ => None
    }

    val decoder: Decoder[Delta[Option[A]]] = Decoder.instance { c =>
      val o = c.downField("OptionDelta")
      for {
        delta <- o.downField("delta").as[Delta[A]](dCodecA.decoder)
      } yield OptionDelta[A](delta)
    }
  }

  def prism[A, B]
  (name: String, prism: Prism[A, B])
  (implicit partialBCodec: DeltaCodec[B]): DeltaCodec[A] = new DeltaCodec[A] {

    val encoder: PartialEncoder[Delta[A]] = {
      case PrismDelta(p, d) if p == prism =>
        //Note we know that delta is a Delta[B] since PrismDelta has the same prism used to
        //create this instance, which was Prism[A, B]
        partialBCodec.encoder(d.asInstanceOf[Delta[B]])
          .map(dJson =>
            Json.obj(
              "PrismDelta" -> Json.obj(
                name -> dJson
              )
            )
          )
      case _ => None
    }

    val decoder: Decoder[Delta[A]] = Decoder.instance {
      c => c.downField("PrismDelta")
        .downField(name)
        .as[Delta[B]](partialBCodec.decoder)
        .map(db => PrismDelta(prism, db))
    }
  }


  def action[M, A <: Delta[M]](name: String)(implicit ct: ClassTag[A], encodeA: Encoder[A], decodeA: Decoder[A]): DeltaCodec[M] = new DeltaCodec[M] {

    val encoder: PartialEncoder[Delta[M]] = {
      case ct(a) =>
        Some(
          Json.obj(
            "ActionDelta" -> Json.obj(
              name -> encodeA(a)
            )
          )
        )
      case _ => None
    }

    val decoder: Decoder[Delta[M]] = Decoder.instance(c =>
      //Expect an object with a value field, containing an encoded M instance. We
      //map this to a ValueDelta using that instance (i.e. this provides a delta
      //replacing the old instance with the decoded instance)
      c.downField("ActionDelta").downField(name).as[A]

      //Map to Delta[M] for neatness
    ).map(a => a: Delta[M])
  }

  // Not implicit - this is used from Mirror codec builder to make the delta codec to go with plain
  // Codec[Mirror]
  def mirror[A](implicit mirrorCodecA: MirrorCodec[A]): DeltaCodec[Mirror] = new DeltaCodec[Mirror] {

    val encoder: PartialEncoder[Delta[Mirror]] = {
      case MirrorDelta(mirrorType, ref, delta) if mirrorType == mirrorCodecA.mirrorType =>
        // We have checked that the MirrorDelta has the same mirrorType as our codec, so
        // it's delta must have the same data type A
        val deltaJs = mirrorCodecA.deltaCodec.encoder(delta.asInstanceOf[Delta[A]])
        deltaJs.map(
          djs => Json.obj(
            "MirrorDelta" -> Json.obj(
            mirrorCodecA.mirrorType.name -> Json.obj(
              "ref" -> ref.asJson,
              "delta" -> djs
            )
          )
        )
      )
      case x => None
    }

    val decoder: Decoder[Delta[Mirror]] = Decoder.instance(c => {
      // Note we require the json to contain the mirror codec's type name
      val o = c.downField("MirrorDelta").downField(mirrorCodecA.mirrorType.name)
      for {
        ref <- o.downField("ref").as[Ref[A]]
        delta <- mirrorCodecA.deltaCodec.decoder.tryDecode(o.downField("delta"))
      } yield MirrorDelta[A](mirrorCodecA.mirrorType, ref, delta)
    })
  }

//  class DeltaCodecMirror[A](implicit mirrorCodecA: MirrorCodec[A]) extends DeltaCodec[Mirror] {
//
//    val decoder: Decoder[Delta[Mirror]] = Decoder.instance(c => {
//      // Note we require the json to contain the mirror codec's type name
//      val o = c.downField("mirror").downField(mirrorCodecA.mirrorType.name)
//      for {
//        ref <- o.downField("ref").as[Ref[A]]
//        delta <- mirrorCodecA.deltaCodec.tryDecode(o.downField("delta"))
//      } yield MirrorDelta[A](ref, delta)
//    })
//
//    def apply(c: HCursor): Decoder.Result[Delta[Mirror]] = decoder(c)
//  }
//
//  // Not implicit - this is used from Mirror codec builder to make the delta codec to go with plain
//  // Codec[Mirror]
//  def mirror[A](implicit mirrorCodecA: MirrorCodec[A]): DeltaCodec[Mirror] = new DeltaCodecMirror[A]

  implicit def refValueDeltaCodec[M]: DeltaCodec[Ref[M]] = value[Ref[M]]

  implicit val guidDeltaCodec: DeltaCodec[Guid] = value[Guid]

}
