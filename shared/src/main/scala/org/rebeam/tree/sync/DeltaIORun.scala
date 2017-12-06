package org.rebeam.tree.sync

import cats.data.State
import cats.~>
import org.rebeam.tree.Delta._
import org.rebeam.tree._
import org.rebeam.tree.random.PRandom
import org.rebeam.tree.ref.MirrorCodec
import org.rebeam.tree.sync.Sync._

object DeltaIORun {

  case class AddedRef[A](id: Id[A], data: A, revision: Guid, codec: MirrorCodec[A])

  case class DeltaRunResult[A](data: A, addedRefs: List[AddedRef[_]])

  private case class StateData(context: DeltaIOContext, deltaId: DeltaId, currentGuidId: Long, addedRefs: List[AddedRef[_]], pr: PRandom) {
    def withNextGuidId: StateData = copy(currentGuidId = currentGuidId + 1)
    def random[A](f: PRandom => (PRandom, A)): (StateData, A) = {
      val (prNew, a) = f(pr)
      (copy(pr = prNew), a)
    }
  }

  private object StateData {
    def initial(context: DeltaIOContext, deltaId: DeltaId): StateData = {
      val pr = PRandom(deltaId.clientId.id ^ deltaId.clientDeltaId.id)
      StateData(context, deltaId, 0, Nil, pr)
    }
  }

  // State monad using StateData
  private type DeltaContextState[A] = State[StateData, A]

  private val pureCompiler: DeltaIOA ~> DeltaContextState =
    new (DeltaIOA ~> DeltaContextState) {
      def apply[A](fa: DeltaIOA[A]): DeltaContextState[A] =
        fa match {
          case GetGuid => State(s => (s.withNextGuidId, Guid(s.deltaId.clientId, s.deltaId.clientDeltaId, WithinDeltaId(s.currentGuidId))))

          case GetId() => State(s => (s.withNextGuidId, Id[Any](Guid(s.deltaId.clientId, s.deltaId.clientDeltaId, WithinDeltaId(s.currentGuidId)))))

          case GetContext => State(s => (s, s.context))

          case GetDeltaId => State(s => (s, s.deltaId))

          // Random values
          case GetPRInt => State(_.random(_.int))
          case GetPRIntUntil(bound) => State(_.random(_.intUntil(bound)))
          case GetPRLong => State(_.random(_.long))
          case GetPRBoolean => State(_.random(_.boolean))
          case GetPRFloat => State(_.random(_.float))
          case GetPRDouble => State(_.random(_.double))


          case Put(create, codec) => State(s => {
            val id = Id[A](Guid(s.deltaId.clientId, s.deltaId.clientDeltaId, WithinDeltaId(s.currentGuidId)))
            val revision = Guid(s.deltaId.clientId, s.deltaId.clientDeltaId, WithinDeltaId(s.currentGuidId + 1))
            // MAke the data item
            val createDIO: DeltaIO[A] = create(id)
            // Remember to increment guid twice for the guid and revision we generated
            val stateWithA = createDIO.foldMap(pureCompiler).run(s.withNextGuidId.withNextGuidId).value

            val newAddedRefs = AddedRef(id, stateWithA._2, revision, codec) :: stateWithA._1.addedRefs
            (stateWithA._1.copy(addedRefs = newAddedRefs), stateWithA._2)
          })
        }
    }

  def runDeltaIO[A](dc: DeltaIO[A], context: DeltaIOContext, deltaId: DeltaId): DeltaRunResult[A] = {
    val s = dc.foldMap(pureCompiler).run(StateData.initial(context, deltaId)).value
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
