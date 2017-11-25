package org.rebeam.tree

import org.rebeam.tree.sync.{Guid, Ref}

import scala.language.implicitConversions
import shapeless._

import scala.collection.immutable.Set

// Evidence that an A is something that we can look around in for Qs that
// satisfy some predicate, where we will collect all found Q's in a Set
trait Searchable[A, Q] {
  def find(p: Q => Boolean)(a: A): Set[Q]
}

trait LowPrioritySearchable {
  implicit def hlistishSearchable[A, L <: HList, Q](
    implicit gen: Generic.Aux[A, L], s: Searchable[L, Q]
  ): Searchable[A, Q] = new Searchable[A, Q] {
    def find(p: Q => Boolean)(a: A) = s.find(p)(gen to a)
  }

  import Searchable.notSearchable

  // Provide low-priority searchables for values, which will generally
  // only be searched for directly, rather than recursively (as we do for 
  // lists, products etc.). When Q is one of these types, elemSearchable will
  // be found as a higher priority implicit, and this will apply the predicate
  // directly to the value.
  implicit def booleanSearchable[Q]: Searchable[Boolean, Q] = notSearchable
  implicit def byteSearchable[Q]: Searchable[Byte, Q] = notSearchable
  implicit def shortSearchable[Q]: Searchable[Short, Q] = notSearchable
  implicit def intSearchable[Q]: Searchable[Int, Q] = notSearchable
  implicit def longSearchable[Q]: Searchable[Long, Q] = notSearchable
  implicit def floatSearchable[Q]: Searchable[Float, Q] = notSearchable
  implicit def doubleSearchable[Q]: Searchable[Double, Q] = notSearchable
  implicit def charSearchable[Q]: Searchable[Char, Q] = notSearchable
  implicit def stringSearchable[Q]: Searchable[String, Q] = notSearchable
  implicit def symbolSearchable[Q]: Searchable[Symbol, Q] = notSearchable
}

object Searchable extends LowPrioritySearchable {

  /**
    * Defines a Searchable that finds nothing
    */
  def notSearchable[A, Q]: Searchable[A, Q] = new Searchable[A, Q] {
    def find(p: Q => Boolean)(a: A) = Set.empty
  }

  // This ensures that the actual type we are searching for is used directly with predicate,
  // this overrides the low priority implementations if we are looking for e.g. a String.
  implicit def elemSearchable[A]: Searchable[A, A] = new Searchable[A, A] {
    def find(p: A => Boolean)(a: A) = if (p(a)) Set(a) else Set.empty
  }

  implicit def listSearchable[A, Q](implicit s: Searchable[A, Q]): Searchable[List[A], Q] =
    new Searchable[List[A], Q] {
      def find(p: Q => Boolean)(a: List[A]) = a.flatMap(s.find(p)).toSet
    }

  implicit def setSearchable[A, Q](implicit s: Searchable[A, Q]): Searchable[Set[A], Q] =
    new Searchable[Set[A], Q] {
      def find(p: Q => Boolean)(a: Set[A]) = a.flatMap(s.find(p)).toSet
    }

  implicit def hnilSearchable[Q]: Searchable[HNil, Q] = new Searchable[HNil, Q] {
    def find(p: Q => Boolean)(a: HNil) = Set.empty
  }

  implicit def hlistSearchable[H, T <: HList, Q](
    implicit hs: Searchable[H, Q], ts: Searchable[T, Q]
  ): Searchable[H :: T, Q] = new Searchable[H :: T, Q] {
    def find(p: Q => Boolean)(a: H :: T) =
      hs.find(p)(a.head) ++ ts.find(p)(a.tail)
      // If we want to allow compilation with missing typeclasses for types occurring in HLists, we
      // can add a default value of null to hs parameter above, and then use the less safe 
      // version below.
      // However we then risk missing data we SHOULD be searching. The current form requires
      // a typeclass for all types in the data, this can be easily provided for unsearchable
      // types using:
      // ```
      // implicit def someTypeSearchable[Q]: Searchable[SomeType, Q] = notSearchable
      // ```
      // The less safe version:
      // If we have a searchable for head, use it, and add the results of searching the tail
      // Option(hs).fold(Set.empty[Q])(_.find(p)(a.head)) ++ ts.find(p)(a.tail)
  }

  implicit def refSearchableForGuid[A]: Searchable[Ref[A], Guid] = new Searchable[Ref[A], Guid] {
    def find(p: Guid => Boolean)(a: Ref[A]): Set[Guid] = if (p(a.id.guid)) Set(a.id.guid) else Set.empty
  }

  case class SearchableWrapper[A](a: A) {
    def deepFind[Q](p: Q => Boolean)(implicit s: Searchable[A, Q]): Set[Q] =
      s.find(p)(a)

    def allRefGuids(implicit s: Searchable[A, Guid]): Set[Guid] =
      s.find(_ => true)(a)
  }

  implicit def wrapSearchable[A](a: A): SearchableWrapper[A] = SearchableWrapper(a)

}
