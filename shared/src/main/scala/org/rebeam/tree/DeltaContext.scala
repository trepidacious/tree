package org.rebeam.tree

import org.rebeam.tree.sync.Sync.Guid

import cats.free.Free
import cats.free.Free.liftF

object DeltaContext {

  sealed trait DeltaContextA[A]
  case class GetId[T]() extends DeltaContextA[Guid[T]]

  type DeltaContext[A] = Free[DeltaContextA, A]

  // GetId returns a Guid[T] value.
  def getId[T]: DeltaContext[Guid[T]] =
    liftF[DeltaContextA, Guid[T]](GetId[T]())

  def pure[T](t: T): DeltaContext[T] = Free.pure(t)

}
