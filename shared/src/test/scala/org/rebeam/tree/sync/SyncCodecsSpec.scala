package org.rebeam.tree.sync

import io.circe._
import io.circe.syntax._
import io.circe.generic.JsonCodec
import org.rebeam.lenses.macros.Lenses
import org.rebeam.tree.sync.Sync._
import org.scalatest._
import DeltaIORun._

import scala.language.higherKinds
import org.rebeam.tree._
import BasicDeltaDecoders._
import DeltaCodecs._
import org.rebeam.tree.Delta._
import org.rebeam.tree.sync.ServerStoreUpdate.ServerStoreIncrementalUpdate
import cats.syntax.either._
import org.rebeam.lenses.PrismN

class SyncCodecsSpec extends WordSpec with Matchers {

  @JsonCodec
  @Lenses
  case class Address(streetName: String, number: Int)

  @JsonCodec
  @Lenses
  case class Person(name: String, address: Address)

  @JsonCodec
  sealed trait AddressAction extends Delta[Address]
  object AddressAction {
    case class NumberMultiple(multiple: Int) extends AddressAction {
      def apply(s: Address): DeltaIO[Address] = pure(s.copy(number = s.streetName.length * multiple))
    }

    case object Capitalise extends AddressAction {
      def apply(s: Address): DeltaIO[Address] = pure(s.copy(streetName = s.streetName.toLowerCase.capitalize))
    }
  }

  implicit val addressDeltaDecoder =
    DeltaCodecs.value[Address] or lensN(Address.streetName) or lensN(Address.number) or action[Address, AddressAction]

  implicit val personDeltaDecoder =
    DeltaCodecs.value[Person] or lensN(Person.name) or lensN(Person.address)

  @JsonCodec
  sealed trait Animal {
    def name: String
  }
  object Animal {
    @JsonCodec
    @Lenses
    case class Dog(name: String, barkLevel: Double) extends Animal

    @JsonCodec
    @Lenses
    case class Cat(name: String, ennui: Double) extends Animal

    val dogPrism: PrismN[Animal, Dog] = PrismN.classTag("Dog")
    val catPrism: PrismN[Animal, Cat] = PrismN.classTag("Cat")
  }

  import Animal._

  implicit val dogDeltaDecoder: Decoder[Delta[Dog]] =
    DeltaCodecs.value[Dog]

  implicit val catDeltaDecoder: Decoder[Delta[Cat]] =
    DeltaCodecs.value[Cat]

  implicit val animalDeltaDecoder: Decoder[Delta[Animal]] =
    DeltaCodecs.value[Animal] or prismN(dogPrism) or prismN(catPrism)


  //A delta and js encoding
  val delta = LensNDelta(Person.address, LensNDelta(Address.streetName, ValueDelta("New Street")))
  val deltaJs = Json.obj(
    "lens" -> Json.obj(
      "lensN" -> Json.obj(
        "address" -> Json.obj(
          "lens" -> Json.obj(
            "lensN" -> Json.obj(
              "streetName" -> Json.obj(
                "value" -> "New Street".asJson
              )
            )
          )
        )
      )
    )
  )

  //Delta and js encoding attempting to edit an animal as a dog
  val dog2 = Dog("Fido2", 2.0)
  val cat2 = Cat("Mittens2", 2.0)
  val animalDogDelta = PrismNDelta[Animal, Dog](Animal.dogPrism, ValueDelta(dog2))
  val animalDogDeltaJs = Json.obj(
    "prism" -> Json.obj(
      "prismN" -> Json.obj(
        "Dog" -> Json.obj(
          "value" -> dog2.asJson
        )
      )
    )
  )

  val animalCatDelta = PrismNDelta[Animal, Cat](Animal.catPrism, ValueDelta(cat2))
  val animalCatDeltaJs = Json.obj(
    "prism" -> Json.obj(
      "prismN" -> Json.obj(
        "Cat" -> Json.obj(
          "value" -> cat2.asJson
        )
      )
    )
  )

  "prismN delta decoder" should {
    "decode via deltas using prisms" in {
      assert(animalDeltaDecoder.decodeJson(animalDogDeltaJs).toOption.contains(animalDogDelta))
      assert(animalDeltaDecoder.decodeJson(animalCatDeltaJs).toOption.contains(animalCatDelta))
    }
  }

  "Sync codecs" should {
    "encode and decode client message" in {
      val p = Person("Ada", Address("Street", 1))
      val id = DeltaId(ClientId(123), ClientDeltaId(456))

      val p2 = delta.runWithIdAndA(id, p)

      assert(p2 == Person("Ada", Address("New Street", 1)))


      val dij = DeltaWithIJ[Person](delta, id, deltaJs)

      val dijJs = clientMsgEncoder[Person](dij)

      val dij2 = clientMsgDecoder[Person].decodeJson(dijJs)

      dij2.fold(
        fail(_),
        d => assert (dij == d)
      )

    }

    "decode incremental update" in {

      val deltaId = DeltaId(ClientId(99), ClientDeltaId(100))
      val update = ServerStoreIncrementalUpdate[Person](
        ModelId(123),
        Vector(
          DeltaWithIJ[Person](delta, deltaId, deltaJs)
        ),
        ModelId(456)
      )

      val encodedAsRemoteUpdate = serverStoreUpdateEncoder[Person](ClientId(0)).apply(update)
      val decodedAsRemoteUpdate = updateDecoder[Person].decodeJson(encodedAsRemoteUpdate)

      decodedAsRemoteUpdate fold (
        fail(_),
        {
          case i@ModelIncrementalUpdate(_,_,_) =>
            assert(i.baseModelId == ModelId(123))
            assert(i.updatedModelId == ModelId(456))
            assert(i.deltas == Vector(
              RemoteDelta(delta, deltaId)
            ))
          case _ => fail("Incremental update decoded to full")
        }

      )

      val encodedAsLocalUpdate = serverStoreUpdateEncoder[Person](ClientId(99)).apply(update)
      val decodedAsLocalUpdate = updateDecoder[Person].decodeJson(encodedAsLocalUpdate)

      decodedAsLocalUpdate fold (
        fail(_),
        {
          case i@ModelIncrementalUpdate(_,_,_) =>
            assert(i.baseModelId == ModelId(123))
            assert(i.updatedModelId == ModelId(456))
            assert(i.deltas == Vector(
              LocalDelta(deltaId)
            ))
          case _ => fail("Incremental update decoded to full")
        }

        )

    }
  }

}
