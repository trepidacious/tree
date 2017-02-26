package org.rebeam.tree.sync

import cats.data.State
import cats.~>
import org.rebeam.tree.Delta._
import org.rebeam.tree.{Delta, DeltaContextA, GetId}
import org.rebeam.tree.sync.Sync._

object DeltaIORun {

  //Which delta we are in and which id we are up to
  private type DeltaContextState[A] = State[(DeltaId, Long), A]

  private val pureCompiler: DeltaContextA ~> DeltaContextState =
    new (DeltaContextA ~> DeltaContextState) {
      def apply[A](fa: DeltaContextA[A]): DeltaContextState[A] =
        fa match {
          case GetId() => State(s => ((s._1, s._2 + 1), Guid[Any](s._1.clientId, s._1.clientDeltaId, s._2)))
        }
    }

  def runDeltaIO[A](dc: DeltaIO[A], deltaId: DeltaId): A =
    dc.foldMap(pureCompiler).run((deltaId, 0)).value._2

  implicit class DeltaRun[A](d: Delta[A]) {
    def runWithIdAndA(id: DeltaId, a: A): A = runDeltaIO(d(a), id)
  }

  implicit class DeltaIORun[A](dio: DeltaIO[A]) {
    def runWithId(id: DeltaId): A = runDeltaIO(dio, id)
  }

  implicit class DeltaAndIdRun[A](deltaAndId: DeltaAndId[A]) {
    def runWithA(a:A): A = runDeltaIO(deltaAndId.delta.apply(a), deltaAndId.id)
  }

  implicit class DeltaWithIJRun[A](dij: DeltaWithIJ[A]) {
    def runWithA(a:A): A = runDeltaIO(dij.delta.apply(a), dij.id)
  }
}
