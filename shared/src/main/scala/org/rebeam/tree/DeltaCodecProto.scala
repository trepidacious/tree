package org.rebeam.tree


import org.rebeam.lenses.macros.Lenses
import Delta._
import org.rebeam.lenses.LensN

import scala.language.higherKinds
import monocle.Lens

import io.circe._, io.circe.parser._, io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.generic.JsonCodec

import org.rebeam.tree.sync.DeltaIORun._
import org.rebeam.tree.sync.Sync._
import cats.syntax.either._

object DeltaCodecProto {

  @JsonCodec
  case class StringValueDelta(v: String) extends Delta[String] {
    def apply(a: String): DeltaIO[String] = pure(v)
  }

  @JsonCodec
  case class IntValueDelta(v: Int) extends Delta[Int] {
    def apply(a: Int): DeltaIO[Int] = pure(v)
  }

  object LensDelta {
    def byLens[A, B](a: A, lens: Lens[A, B], delta: Delta[B]): DeltaIO[A] = delta(lens.get(a)).map(lens.set(_)(a))
  }

  abstract class LensDelta[A, B](lens: Lens[A, B], d: Delta[B]) extends Delta[A] {
    def apply(a: A): DeltaIO[A] = LensDelta.byLens(a, lens, d)
  }

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

    import org.rebeam.tree.sync.Sync.Guid._

    object ListGuidDelta {
      implicit final def encoder[A: ToId: Encoder]: Encoder[ListGuidDelta[A]] = deriveEncoder
      implicit final def decoder[A: ToId: Decoder]: Decoder[ListGuidDelta[A]] = deriveDecoder
    }

    case class ListGuidDelta[A: ToId](id: Guid[A], a: A) extends ListDelta[A] {
      def apply(l: List[A]): DeltaIO[List[A]] = {
        val i = l.indexWhere(implicitly[ToId[A]].id(_) == id)
        if (i >= 0 && i < l.size) {
          pure(l.updated(i, a))
        } else {
          pure(l)
        }
      }
    }

  }

  @JsonCodec
  @Lenses
  case class Person(name: String, age: Int)

  @JsonCodec
  @Lenses
  case class Group(members: List[Person])

  @JsonCodec
  sealed trait PersonDelta extends Delta[Person]
  object PersonDelta {
    case class Name(d: StringValueDelta) extends LensDelta(Person.name, d) with PersonDelta
    case class Age(d: IntValueDelta) extends LensDelta(Person.age, d) with PersonDelta
  }

  // @JsonCodec
  // case class GroupDelta(d: ) extends LensDelta(Group.members, )

  def main(args: Array[String]): Unit = {
  
    val a = Person("Alice", 100)
    val rename: PersonDelta = PersonDelta.Name(StringValueDelta("Bob"))
    println(rename.asJson)
    println(decode[PersonDelta](rename.asJson.noSpaces))
    
    val renameA = rename(a)

    val b = renameA.runWith(
      DeltaIOContext(Moment(0)),
      DeltaId(ClientId(0), ClientDeltaId(0))
    ).data

    println(s"$rename: $a -> $b")

  }

}
