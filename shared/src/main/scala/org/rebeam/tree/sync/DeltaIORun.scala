package org.rebeam.tree.sync

import cats.data.State
import cats.~>
import org.rebeam.tree.Delta._
import org.rebeam.tree._
import org.rebeam.tree.sync.Sync._

object DeltaIORun {

  private case class StateData(context: DeltaIOContext, deltaId: DeltaId, currentGuidId: Long) {
    def withNextGuidId: StateData = copy(currentGuidId = currentGuidId + 1)
  }

  // State monad using StateData
  private type DeltaContextState[A] = State[StateData, A]

  private val pureCompiler: DeltaIOA ~> DeltaContextState =
    new (DeltaIOA ~> DeltaContextState) {
      def apply[A](fa: DeltaIOA[A]): DeltaContextState[A] =
        fa match {
          case GetId() => State(s => (s.withNextGuidId, Guid[Any](s.deltaId.clientId, s.deltaId.clientDeltaId, s.currentGuidId)))

          case GetContext => State(s => (s, s.context))
        }
    }

  def runDeltaIO[A](dc: DeltaIO[A], context: DeltaIOContext, deltaId: DeltaId): A =
    dc.foldMap(pureCompiler).run(StateData(context, deltaId, 0)).value._2

  implicit class DeltaRun[A](d: Delta[A]) {
    def runWith(a: A, context: DeltaIOContext, deltaId: DeltaId): A = runDeltaIO(d(a), context, deltaId)
  }

  implicit class DeltaIORun[A](dio: DeltaIO[A]) {
    def runWith(context: DeltaIOContext, deltaId: DeltaId): A = runDeltaIO(dio, context, deltaId)
  }

  implicit class DeltaAndIdRun[A](deltaAndId: DeltaAndId[A]) {
    def runWith(a: A, context: DeltaIOContext): A = runDeltaIO(deltaAndId.delta.apply(a), context, deltaAndId.id)
  }

  implicit class DeltaWithIJRun[A](dij: DeltaWithIJ[A]) {
    def runWith(a: A, context: DeltaIOContext): A = runDeltaIO(dij.delta.apply(a), context, dij.id)
  }

  implicit class DeltaWithIJCRun[A](dijc: DeltaWithIJC[A]) {
    def runWith(a: A): A = runDeltaIO(dijc.delta.apply(a), dijc.context, dijc.id)
  }

  implicit class DeltaWithICRun[A](dic: DeltaWithIC[A]) {
    def runWith(a: A): A = runDeltaIO(dic.delta.apply(a), dic.context, dic.id)
    def runWithNewContext(a: A, context: DeltaIOContext): A = runDeltaIO(dic.delta.apply(a), context, dic.id)
  }
}
