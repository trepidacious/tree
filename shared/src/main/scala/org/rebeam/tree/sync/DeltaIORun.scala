package org.rebeam.tree.sync

import cats.data.State
import cats.~>
import org.rebeam.tree.Delta._
import org.rebeam.tree._
import org.rebeam.tree.sync.Sync._

object DeltaIORun {

  private case class StateData[C](context: DeltaIOContext, deltaId: DeltaId, currentGuidId: Long, data: Map[Guid[C], C]) {
    def withNextGuidId: StateData[C] = copy(currentGuidId = currentGuidId + 1)
    def putData(id: Guid[C], c: C): StateData[C] = copy(data = data.updated(id, c))
  }

  // State monad using StateData
  private type DeltaContextState[C, A] = State[StateData[C], A]

  private def pureCompiler[C]: DeltaIOA ~> DeltaContextState =
    new (DeltaIOA ~> DeltaContextState) {
      def apply[A](fa: DeltaIOA[C, A]): DeltaContextState[C, A] =
        fa match {
          case GetId() => State(s => (s.withNextGuidId, Guid[Any](s.deltaId.clientId, s.deltaId.clientDeltaId, s.currentGuidId)))

          case GetContext => State(s => (s, s.context))

          case PutData(id, c) => State(s => (s.putData(id, c), ()))
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
