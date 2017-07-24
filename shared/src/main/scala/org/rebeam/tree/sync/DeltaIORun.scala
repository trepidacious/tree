package org.rebeam.tree.sync

import cats.data.State
import cats.~>
import org.rebeam.tree.Delta._
import org.rebeam.tree._
import org.rebeam.tree.ref.MirrorCodec
import org.rebeam.tree.sync.Sync._

object DeltaIORun {

  case class AddedRef[A](id: Guid[A], data: A, revision: Guid[A], codec: MirrorCodec[A])

  case class DeltaRunResult[A](data: A, addedRefs: List[AddedRef[_]])

  private case class StateData(context: DeltaIOContext, deltaId: DeltaId, currentGuidId: Long, addedRefs: List[AddedRef[_]]) {
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

          case Put(create, codec) => State(s => {
            val guid = Guid[A](s.deltaId.clientId, s.deltaId.clientDeltaId, s.currentGuidId)
            val revision = Guid[A](s.deltaId.clientId, s.deltaId.clientDeltaId, s.currentGuidId + 1)
            val createDIO: DeltaIO[A] = create(guid)
            // Remember to increment guid twice for the guid and revision we generated
            val stateWithA = createDIO.foldMap(pureCompiler).run(s.withNextGuidId.withNextGuidId).value

            val newAddedRefs = AddedRef(guid, stateWithA._2, revision, codec) :: stateWithA._1.addedRefs
            (stateWithA._1.copy(addedRefs = newAddedRefs), stateWithA._2)
          })
        }
    }

  def runDeltaIO[A](dc: DeltaIO[A], context: DeltaIOContext, deltaId: DeltaId): DeltaRunResult[A] = {
    val s = dc.foldMap(pureCompiler).run(StateData(context, deltaId, 0, Nil)).value
    DeltaRunResult(s._2, s._1.addedRefs)
  }


  implicit class DeltaRun[A](d: Delta[A]) {
    def runWith(a: A, context: DeltaIOContext, deltaId: DeltaId): DeltaRunResult[A] = runDeltaIO(d(a), context, deltaId)
  }

  implicit class DeltaIORun[A](dio: DeltaIO[A]) {
    def runWith(context: DeltaIOContext, deltaId: DeltaId): DeltaRunResult[A] = runDeltaIO(dio, context, deltaId)
  }

  implicit class DeltaAndIdRun[A](deltaAndId: DeltaAndId[A]) {
    def runWith(a: A, context: DeltaIOContext): DeltaRunResult[A] = runDeltaIO(deltaAndId.delta.apply(a), context, deltaAndId.id)
  }

  implicit class DeltaWithIJRun[A](dij: DeltaWithIJ[A]) {
    def runWith(a: A, context: DeltaIOContext): DeltaRunResult[A] = runDeltaIO(dij.delta.apply(a), context, dij.id)
  }

  implicit class DeltaWithIJCRun[A](dijc: DeltaWithIJC[A]) {
    def runWith(a: A): DeltaRunResult[A] = runDeltaIO(dijc.delta.apply(a), dijc.context, dijc.id)
  }

  implicit class DeltaWithICRun[A](dic: DeltaWithIC[A]) {
    def runWith(a: A): DeltaRunResult[A] = runDeltaIO(dic.delta.apply(a), dic.context, dic.id)
    def runWithNewContext(a: A, context: DeltaIOContext): DeltaRunResult[A] = runDeltaIO(dic.delta.apply(a), context, dic.id)
  }
}
