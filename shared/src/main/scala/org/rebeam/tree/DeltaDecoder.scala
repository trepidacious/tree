package org.rebeam.tree

import io.circe._
import monocle._

import scala.language.higherKinds

object DeltaDecoder {

  def value[M: Decoder]: Decoder[Delta[M]] = Decoder.instance(c =>
    //Expect an object with a value field, containing an encoded M instance. We
    //map this to a ValueDelta using that instance (i.e. this provides a delta
    //replacing the old instance with the decoded instance)
    c.downField("value").as[M].map(ValueDelta(_))
  )

  def lens[M, B](lensName: String, lens: Lens[M, B])(implicit deltaDecoder: Decoder[Delta[B]]): Decoder[Delta[M]] = Decoder.instance(c =>
    //This decodes a LensDelta on a data item of type M, using a lens from M to B.
    //The top level object has a "lens" field as a type identifier, with value encoding the delta.
    //We then require an object with a "lensName" field, if we find this we
    //decode the value of that field as a Delta[B]. Finally, we wrap this
    //Delta[B] in a LensDelta[M, B] to make it into a Delta[M].
    c.downField("lens").downField(lensName).as[Delta[B]].map(LensDelta(lens, _))
  )

  def action[M, A <: Delta[M] : Decoder]: Decoder[Delta[M]] = Decoder.instance(c =>
    //Expect an object with a value field, containing an encoded M instance. We
    //map this to a ValueDelta using that instance (i.e. this provides a delta
    //replacing the old instance with the decoded instance)
    c.downField("action").as[A]

  //Map to Delta[M] for neatness
  ).map(a => a: Delta[M])

}
