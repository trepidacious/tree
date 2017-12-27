package org.rebeam.tree

import cats.free.Free
import cats.free.Free.liftF
import cats.~>
import io.circe.generic.JsonCodec
import org.rebeam.lenses._
import monocle._
import monocle.std.option
import org.rebeam.tree.Delta._
import org.rebeam.tree.sync.Sync._
import org.rebeam.tree.sync._

trait Delta[U, A] {
  def apply(a: A): DeltaIO[U, A]
}

sealed trait DeltaIOA[U, A]
case class GetGuid[U]() extends DeltaIOA[U, Guid]
case class GetId[U, T <: U]() extends DeltaIOA[U, Id[T]]
case class GetContext[U]() extends DeltaIOA[U, DeltaIOContext]
case class GetDeltaId[U]() extends DeltaIOA[U, DeltaId]
case class GetPRInt[U]() extends DeltaIOA[U, Int]
case class GetPRIntUntil[U](bound: Int) extends DeltaIOA[U, Int]
case class GetPRLong[U]() extends DeltaIOA[U, Long]
case class GetPRBoolean[U]() extends DeltaIOA[U, Boolean]
case class GetPRFloat[U]() extends DeltaIOA[U, Float]
case class GetPRDouble[U]() extends DeltaIOA[U, Double]
case class Put[U, T <: U](create: Id[T] => DeltaIO[U, T]) extends DeltaIOA[U, T]

/**
  * Provides the context within which an individual run of a DeltaIO can
  * occur. Currently just the moment in which we should run, but may be extended
  * in future.
  * @param moment The moment in which this DeltaIO is running.
  *  When run on the client this is provisional,
  *  on the server it is authoritative - the authoritative
  *  value is returned back to the client when the delta
  *  is applied on the server, so the client can rerun
  *  with the correct moment.)
  */
@JsonCodec
case class DeltaIOContext(moment: Moment)


/**
  * Provides DeltaIOContext for executing DeltaIOs.
  * Note that this has side effects - e.g. reading
  * the current time. This should be used carefully.
  */
trait DeltaIOContextSource {
  def getContext: DeltaIOContext
}

object DeltaIOContextSource {
  val default: DeltaIOContextSource = new DeltaIOContextSource {
    override def getContext: DeltaIOContext = DeltaIOContext(Moment(System.currentTimeMillis()))
  }
}

object Delta {

  type DeltaIO[U, A] = Free[({ type T[X] = DeltaIOA[U, X] })#T, A]

//  type DeltaION[A] = DeltaIO[Nothing, A]
//
//  type DeltaN[A] = Delta[Nothing, A]

//  class Widener[U, V <: U] {
//    type DIOU[A] = DeltaIOA[U, A]
//    type DIOV[A] = DeltaIOA[V, A]
//
//    val widenUArrow: DIOV ~> DIOU =
//      new (DIOV ~> DIOU) {
//        def apply[A](v: DIOV[A]): DIOU[A] =
//          v match {
//            case GetGuid() => GetGuid()
//            case GetId() => GetId()
//            case GetContext() => GetContext()
//            case GetDeltaId() => GetDeltaId()
//            case GetPRInt() => GetPRInt()
//            case GetPRIntUntil(bound) => GetPRIntUntil(bound)
//            case GetPRLong() => GetPRLong()
//            case GetPRBoolean() => GetPRBoolean()
//            case GetPRFloat() => GetPRFloat()
//            case GetPRDouble() => GetPRDouble()
//            case Put(create) =>
//              val createU = ((a: Id[A] ) => a.asInstanceOf[Id[V with A]]).andThen(create.andThen(dioV => Delta.widenU[U, V, A](dioV.map(vWithA => vWithA : A))))
//              Put[U, A](createU)
//          }
//      }
//  }

  // FIXME implement using mapK and DeltaIOA[V, A] ~> DeltaIOA[U, A]
  def widenU[U, V <: U, A](d: DeltaIO[V, A]): DeltaIO[U, A] = {
    d.asInstanceOf[DeltaIO[U, A]]
  }

  /**
    * GetId returns an Id[T] value.
    * @tparam T The type for which we need an Id
    * @return A new Id for the given type. Use for
    *         only one data item - do not reuse.
    */
  def getId[U, T <: U]: DeltaIO[U, Id[T]] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, Id[T]](GetId[U, T]())

  /**
    * GetGuid returns a Guid value.
    * @return A new Guid.
    */
  def getGuid[U]: DeltaIO[U, Guid] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, Guid](GetGuid[U]())

  /**
    * Get the DeltaIOContext within which the DeltaIO
    * is running, for example giving the current time
    * as a Moment.
    */
  def getContext[U]: DeltaIO[U, DeltaIOContext] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, DeltaIOContext](GetContext[U]())

  /**
    * Get the DeltaId the DeltaIO will produce
    */
  def getDeltaId[U]: DeltaIO[U, DeltaId] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, DeltaId](GetDeltaId[U]())

  /**
    * Get a pseudo-random Int. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  def getPRInt[U]: DeltaIO[U, Int] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, Int](GetPRInt[U]())

  /**
    * Get a pseudo-random Int between 0 and bound - 1.
    * Produced as if by Random initialised using delta id
    * and then used for all `getPR` calls in that delta.
    * This means the sequence of values will be deterministic
    * for a given DeltaId.
    */
  def getPRIntUntil[U](bound: Int): DeltaIO[U, Int] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, Int](GetPRIntUntil[U](bound))

  /**
    * Get a pseudo-random Int. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  def getPRLong[U]: DeltaIO[U, Long] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, Long](GetPRLong[U]())

  /**
    * Get a pseudo-random Boolean. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  def getPRBoolean[U]: DeltaIO[U, Boolean] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, Boolean](GetPRBoolean[U]())

  /**
    * Get a pseudo-random Float. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  def getPRFloat[U]: DeltaIO[U, Float] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, Float](GetPRFloat[U]())

  /**
    * Get a pseudo-random Double. Produced as if by Random
    * initialised using delta id and then used for all
    * `getPR` calls in that delta. This means the sequence
    * of values will be deterministic for a given DeltaId.
    */
  def getPRDouble[U]: DeltaIO[U, Double] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, Double](GetPRDouble[U]())

  /**
    * Put a new data item into the Mirror, where that data item
    * can be created by a DeltaIO using a new Guid.
    * This atomically produces a new Id for a data item, creates
    * that data item, and registers the data item to the Mirror.
    * @tparam T       The type of data item
    * @param create   A function accepting a new Guid for the data item,
    *                 and returning a DeltaIO that will make the data item.
    * @return         A DeltaIO yielding the new item
    */
  def put[U, T <: U](create: Id[T] => DeltaIO[U, T]): DeltaIO[U, T] =
    liftF[({ type T[X] = DeltaIOA[U, X] })#T, T](Put[U, T](create))

  /**
    * Put a new data item into the Mirror, where that data item
    * can be created directly using a new Guid.
    * This atomically produces a new Guid for a data item, creates
    * that data item, and registers the data item to the Mirror.
    * @tparam T       The type of data item
    * @param create   A function accepting a new Guid for the data item,
    *                 and returning a data item.
    * @return         The new item
    */
  def putPure[U, T <: U](create: Id[T] => T): DeltaIO[U, T] =
    put[U, T]((id: Id[T]) => pure(create(id)))

  /**
    * Pure DeltaIO value
    * @param t    The value
    * @tparam T   The type of value
    * @return     DeltaIO returning value directly
    */
  def pure[U, T](t: T): DeltaIO[U, T] =
    Free.pure[({ type T[X] = DeltaIOA[U, X] })#T, T](t)


}

object LensDelta {
  def byLens[U, A, B](a: A, lens: Lens[A, B], delta: Delta[U, B]): DeltaIO[U, A] = delta(lens.get(a)).map(lens.set(_)(a))
}

//abstract class LensDelta[A, B, D <: Delta[B]](lens: Lens[A, B], d: D) extends Delta[A] {
//  def apply(a: A): DeltaIO[A] = LensDelta.byLens(a, lens, d)
//}

abstract class LensDelta[U, A, B](lens: Lens[A, B], delta: Delta[U, B]) extends Delta[U, A] {
  def apply(a: A): DeltaIO[U, A] = delta(lens.get(a)).map(lens.set(_)(a)) //  lens.modify(delta.apply)(a)
}

abstract class OptionalDelta[U, A, B](optional: Optional[A, B], delta: Delta[U, B]) extends Delta[U, A] {
  def apply(a: A): DeltaIO[U, A] =
    optional.getOption(a).fold(
      pure[U, A](a)
    )(
      delta(_).map(optional.set(_)(a))
    )  //optional.modify(delta.apply)(a)
}

abstract class LensNDelta[U, A, B](lensN: LensN[A, B], delta: Delta[U, B]) extends Delta[U, A] {
  def apply(a: A): DeltaIO[U, A] = delta(lensN.get(a)).map(lensN.set(_)(a)) //lensN.modify(delta.apply)(a)
}

abstract class OptionalIDelta[U, A](optionalI: OptionalI[A], delta: Delta[U, A]) extends Delta[U, List[A]] {
  def apply(a: List[A]): DeltaIO[U, List[A]] =
    optionalI.getOption(a).fold(
      pure[U, List[A]](a)
    )(
      delta(_).map(optionalI.set(_)(a))
    )  //optionalI.modify(delta.apply)(l)
}

abstract class OptionalMatchDelta[U, A, F <: A => Boolean](optionalMatch: OptionalMatch[A, F], delta: Delta[U, A]) extends Delta[U, List[A]] {
  def apply(a: List[A]): DeltaIO[U, List[A]] =
    optionalMatch.getOption(a).fold(
      pure[U, List[A]](a)
    )(
      delta(_).map(optionalMatch.set(_)(a))
    )  //optionalMatch.modify(delta.apply)(l)
}

//abstract class OptionDelta[U, A](delta: Delta[U, A]) extends Delta[U, Option[A]] {
////  private lazy val mod: Option[A] => Option[A] = option.some[A].modify(delta.apply)
//  def apply(a: Option[A]): DeltaIO[U, Option[A]] = option.some[A].getOption(a).fold(
//    pure[U, Option[A]](a)
//  )(
//    delta(_).map(option.some[A].set(_)(a))
//  )  //mod(o)
//}

abstract class PrismNDelta[U, S, A](prismN: PrismN[S, A], delta: Delta[U, A]) extends Delta[U, S] {
  def apply(s: S): DeltaIO[U, S] =
    prismN.getOption(s).fold(
      pure[U, S](s)
    )(
      a => delta(a).map(modifiedA => prismN.set(modifiedA)(s))
    )  //optionalI.modify(delta.apply)(l)
}

//abstract class MirrorDelta[U, A, D <: Delta[U, A]](ref: Ref[A], delta: D) extends Delta[U, Mirror[A, D]] {
//  def apply(mirror: Mirror[A, D]): DeltaIO[U, Mirror[A, D]] =
//    mirror(ref).fold(
//      // Data is not in mirror, nothing to do
//      pure(mirror)
//    )(
//      // Data is in mirror, apply delta to it
//      a => for {
//        modifiedA <- delta(a)
//        updatedMirror <- mirror.updated(ref.id, modifiedA)
//      } yield updatedMirror
//    )
//}

abstract class AbstractValueDelta[A](v: A) extends Delta[Nothing, A] {
  def apply(a: A): DeltaIO[Nothing, A] = pure(v)
}

@JsonCodec
case class ValueDelta[A](v: A) extends Delta[Nothing, A] {
  def apply(a: A): DeltaIO[Nothing, A] = pure(v)
}

object ValueDelta {
  type BooleanValueDelta = ValueDelta[Boolean]
  def BooleanValueDelta(a: Boolean): BooleanValueDelta = ValueDelta(a)

  type ByteValueDelta = ValueDelta[Byte]
  def ByteValueDelta(a: Byte): ByteValueDelta = ValueDelta(a)

  type ShortValueDelta = ValueDelta[Short]
  def ShortValueDelta(a: Short): ShortValueDelta = ValueDelta(a)

  type IntValueDelta = ValueDelta[Int]
  def IntValueDelta(a: Int): IntValueDelta = ValueDelta(a)

  type LongValueDelta = ValueDelta[Long]
  def LongValueDelta(a: Long): LongValueDelta = ValueDelta(a)

  type FloatValueDelta = ValueDelta[Float]
  def FloatValueDelta(a: Float): FloatValueDelta = ValueDelta(a)

  type DoubleValueDelta = ValueDelta[Double]
  def DoubleValueDelta(a: Double): DoubleValueDelta = ValueDelta(a)

  type CharValueDelta = ValueDelta[Char]
  def CharValueDelta(a: Char): CharValueDelta = ValueDelta(a)

  type StringValueDelta = ValueDelta[String]
  def StringValueDelta(a: String): StringValueDelta = ValueDelta(a)
}

