package org.rebeam.tree.demo

import io.circe.generic.JsonCodec
import org.rebeam.tree.Searchable._

object SearchableDemo extends App {

  // An example predicate:
  val p = (_: String) endsWith "o"

  // On strings:
  assert("hello".deepFind(p) == Set("hello"))
  assert("hell".deepFind(p) == Set.empty[String])

  // On lists:
  assert(List("yes", "maybe", "no").deepFind(p) == Set("no"))

  // On arbitrarily sized and nested tuples:
  assert(("yes", "maybe", ("no", "why")).deepFind(p) == Set("no"))
  assert(("a", ("b", "c"), "d").deepFind(p) == Set.empty[String])

  // On tuples with non-string elements:
  assert((1, "two", ('three, '4')).deepFind(p) == Set("two"))

  // Search the same tuple for a specific character instead:
  assert((1, "two", ('three, '4')).deepFind((_: Char) == '4') == Set('4'))

  // Our case class:
  case class Foo(a: String, b: String, c: List[String])

  // And it works:
  assert(Foo("four", "three", List("two", "one")).deepFind(p) == Set("two"))
  assert(Foo("a", "b", "c" :: Nil).deepFind(p) == Set.empty[String])

  println(Foo("fouro", "three", List("two", "one")).deepFind(p))

  println(Some("fouro").deepFind(p))

  println(Set("fouro").deepFind(p))

  // Sealed trait - Coproduct
  @JsonCodec
  sealed trait Animal{
    def says: String
  }
  object Animal {
    final case class Cat(name: String) extends Animal {
      def says: String = "Meow!"
    }
    final case class Dog(name: String) extends Animal {
      def says: String = "Moo!"
    }
  }

  import Animal._

  val catAnimal: Animal = Cat("Fluffo")
  println(catAnimal.deepFind(p))
}