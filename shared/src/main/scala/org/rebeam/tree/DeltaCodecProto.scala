package org.rebeam.tree


import org.rebeam.lenses.macros.Lenses
import Delta._

import scala.language.higherKinds

import io.circe._
import io.circe.generic.semiauto._

import org.rebeam.tree.sync.Sync._
import org.rebeam.tree.sync._

import BasicDeltas._

import Searchable._

object DeltaCodecProto {

  sealed trait ListDelta[A] extends Delta[List[A]]

  object ListDelta {

    object ListIndexDelta {
      implicit final def encoder[A: Encoder]: Encoder[ListIndexDelta[A]] = deriveEncoder
      implicit final def decoder[A: Decoder]: Decoder[ListIndexDelta[A]] = deriveDecoder
    }

    case class ListIndexDelta[A](i: Int, a: A) extends ListDelta[A] {
      def apply(l: List[A]): DeltaIO[List[A]] = {
        if (i >= 0 && i < l.size) {
          pure(l.updated(i, a))
        } else {
          pure(l)
        }
      }
    }

    object ListGuidDelta {
      implicit final def encoder[A: Identifiable: Encoder]: Encoder[ListGuidDelta[A]] = deriveEncoder
      implicit final def decoder[A: Identifiable: Decoder]: Decoder[ListGuidDelta[A]] = deriveDecoder
    }

    case class ListGuidDelta[A: Identifiable](id: Id[A], a: A) extends ListDelta[A] {
      def apply(l: List[A]): DeltaIO[List[A]] = {
        val i = l.indexWhere(implicitly[Identifiable[A]].id(_) == id)
        if (i >= 0 && i < l.size) {
          pure(l.updated(i, a))
        } else {
          pure(l)
        }
      }
    }

  }

  // @JsonCodec
  @Lenses
  case class Person(name: String, age: Int, friend: Ref[Person])

  object Person {
    // TODO cache searchable?
    // implicit val personSearchable: Searchable[Person, Ref[_]] = implicitly[Searchable[Person, Ref[_]]]
  }

  // @JsonCodec
  @Lenses
  case class Group(members: List[Person])

  // @JsonCodec
  sealed trait PersonDelta extends Delta[Person]
  object PersonDelta {
    case class Name(d: StringValueDelta) extends LensDelta(Person.name, d) with PersonDelta
    case class Age(d: IntValueDelta) extends LensDelta(Person.age, d) with PersonDelta
    case class Friend(f: Person) extends ValueDelta(f) with PersonDelta
  }

  // @JsonCodec
  // case class GroupDelta(d: ) extends LensDelta(Group.members, )

  def main(args: Array[String]): Unit = {

//    implicit def refNotSearchable[A, Q]: Searchable[Ref[A], Q] = Searchable.notSearchable
  

    val a = Person("Alice", 100, Ref(Id[Person](Guid(ClientId(0), ClientDeltaId(1), WithinDeltaId(2)))))

    // val rename: PersonDelta = PersonDelta.Name(StringValueDelta("Bob"))
    // println(rename.asJson)
    // println(decode[PersonDelta](rename.asJson.noSpaces))
    
    // val renameA = rename(a)

    // val b = renameA.runWith(
    //   DeltaIOContext(Moment(0)),
    //   DeltaId(ClientId(0), ClientDeltaId(0))
    // ).data

    // println(s"$rename: $a -> $b")

    // println(a.deepFind[Ref[Person]](_ => true))

    println(a.allRefGuids)

  }

}
