package org.rebeam.tree

import io.circe._
import org.rebeam.lenses._
import cats.syntax.either._

import scala.language.higherKinds
import scala.reflect.ClassTag

object DeltaCodecs {

  def value[M: Decoder]: Decoder[Delta[M]] = Decoder.instance(c =>
    //Expect an object with a value field, containing an encoded M instance. We
    //map this to a ValueDelta using that instance (i.e. this provides a delta
    //replacing the old instance with the decoded instance)
    c.downField("value").as[M].map(ValueDelta(_))
  )

//  def lens[M, A](lensName: String, lens: Lens[M, A])(implicit deltaDecoder: Decoder[Delta[A]]): Decoder[Delta[M]] = Decoder.instance(c =>
//    //This decodes a LensDelta on a data item of type M, using a lens from M to B.
//    //The top level object has a "lens" field as a type identifier, with value encoding the delta.
//    //We then require an object with a "lensName" field, if we find this we
//    //decode the value of that field as a Delta[B]. Finally, we wrap this
//    //Delta[B] in a LensDelta[M, B] to make it into a Delta[M].
//    c.downField("lens").downField(lensName).as[Delta[A]].map(LensDelta(lens, _))
//  )

  def lensN[M, A](namedLens: LensN[M, A])(implicit deltaDecoder: Decoder[Delta[A]]): Decoder[Delta[M]] = Decoder.instance(c =>
    //This decodes a LensDelta on a data item of type M, using a lens from M to B.
    //The top level object has a "lens" field as a type identifier, with value encoding the delta.
    //We then require an object with a field matching the lens' name, if we find this we
    //decode the value of that field as a Delta[B]. Finally, we wrap this
    //Delta[B] in a LensNDelta[M, B] to make it into a Delta[M].
    c.downField("lens").downField("lensN").downField(namedLens.name).as[Delta[A]].map(LensNDelta(namedLens, _))
  )

  def optionalI[A](implicit deltaDecoder: Decoder[Delta[A]]): Decoder[Delta[List[A]]] = Decoder.instance(c => {
    val o = c.downField("optional").downField("optionalI")
    for {
      index <- o.downField("index").as[Int]
      delta <- o.downField("delta").as[Delta[A]]
    } yield OptionalIDelta[A](OptionalI(index), delta)
  })

  def optionalMatch[A, F <: A => Boolean](implicit deltaDecoder: Decoder[Delta[A]], cDecoder: Decoder[F]): Decoder[Delta[List[A]]] = Decoder.instance(c => {
    val o = c.downField("optional").downField("optionalMatch")
    for {
      f <- o.downField("find").as[F]
      delta <- o.downField("delta").as[Delta[A]]
    } yield OptionalMatchDelta[A, F](OptionalMatch(f), delta)
  })

  def option[A](implicit deltaDecoder: Decoder[Delta[A]]): Decoder[Delta[Option[A]]] = Decoder.instance(c => {
    val o = c.downField("option")
    for {
      delta <- o.downField("delta").as[Delta[A]]
    } yield OptionDelta[A](delta)
  })

  def prismN[S, A](prismN: PrismN[S, A])(implicit deltaDecoder: Decoder[Delta[A]]): Decoder[Delta[S]] = Decoder.instance(c => {
    c.downField("prism").downField("prismByClass").downField(prismN.name).as[Delta[A]].map(delta => PrismNDelta[S, A](prismN, delta))
  })

  //  implicit def lensNOuterEncoder[A, B]: OuterEncoder[LensN[A, B]] = OuterEncoder.instance(
//    (lensN: LensN[A, B], deltaJs: Json) =>
//      Json.obj("lensN" -> Json.obj(lensN.name -> deltaJs))
//  )

  def action[M, A <: Delta[M] : Decoder]: Decoder[Delta[M]] = Decoder.instance(c =>
    //Expect an object with a value field, containing an encoded M instance. We
    //map this to a ValueDelta using that instance (i.e. this provides a delta
    //replacing the old instance with the decoded instance)
    c.downField("action").as[A]

  //Map to Delta[M] for neatness
  ).map(a => a: Delta[M])

//  def lensNDeltaCodec[A, B](lensN: LensN[A,B])(implicit deltaBEncoder: Encoder[Delta[B]], deltaBDecoder: Decoder[Delta[B]]): Codec[LensNDelta[A, B]] = Codec (
//    Encoder.instance(lnd =>
//      Json.obj(
////        "lens" -> Json.obj(
//          lensN.name -> deltaBEncoder(lnd.delta)
////        )
//      )
//    ),
//
//    Decoder.instance(c =>
//      //This decodes a LensDelta on a data item of type M, using a lens from M to B.
//      //The top level object has a "lens" field as a type identifier, with value encoding the delta.
//      //We then require an object with a field matching the lens' name, if we find this we
//      //decode the value of that field as a Delta[B]. Finally, we wrap this
//      //Delta[B] in a LensNDelta[M, B] to make it into a Delta[M].
////      c.downField("lens").downField(lensN.name).as[Delta[B]].map(LensNDelta(lensN, _))
//      c.downField(lensN.name).as[Delta[B]].map(LensNDelta(lensN, _))
//    )
//  )

}
