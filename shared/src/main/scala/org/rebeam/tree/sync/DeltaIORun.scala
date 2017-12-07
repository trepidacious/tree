package org.rebeam.tree.sync

import cats.data.State
import cats.~>
import org.rebeam.tree.Delta._
import org.rebeam.tree._
import org.rebeam.tree.random.PRandom
import org.rebeam.tree.sync.Sync._

object DeltaIORun {

  case class AddedRef[U](id: Id[U], data: U, revision: Guid)

  /**
    * Result of running a DeltaIO[A]
    * @param data       The data resulting from the run
    * @param addedRefs  The refs added during the run
    * @tparam U         The type of added reference
    * @tparam A         The type of data
    */
  case class DeltaRunResult[U, A](data: A, addedRefs: List[AddedRef[U]])

  private case class StateData[U](context: DeltaIOContext, deltaId: DeltaId, currentGuidId: Long, addedRefs: List[AddedRef[U]], pr: PRandom) {
    def withNextGuidId: StateData[U] = copy(currentGuidId = currentGuidId + 1)
    def random[A](f: PRandom => (PRandom, A)): (StateData[U], A) = {
      val (prNew, a) = f(pr)
      (copy(pr = prNew), a)
    }
  }

  private object StateData {
    def initial[U](context: DeltaIOContext, deltaId: DeltaId): StateData[U] = {
      val pr = PRandom(deltaId.clientId.id ^ deltaId.clientDeltaId.id)
      StateData(context, deltaId, 0, Nil, pr)
    }
  }

  class Compiler[U] {
    // State monad using StateData
    private type DeltaContextState[A] = State[StateData[U], A]

    // Specialise DeltaIOA to "universe" type U
    private type DeltaIOAU[A] = DeltaIOA[U, A]

    val pureCompiler: (DeltaIOAU ~> DeltaContextState) =
      new (DeltaIOAU ~> DeltaContextState) {
        def apply[A](fa: DeltaIOA[U, A]): DeltaContextState[A] =
          fa match {
            case GetGuid() => State(s => (s.withNextGuidId, Guid(s.deltaId.clientId, s.deltaId.clientDeltaId, WithinDeltaId(s.currentGuidId))))

            case GetId() => State(s => (s.withNextGuidId, Id[Any](Guid(s.deltaId.clientId, s.deltaId.clientDeltaId, WithinDeltaId(s.currentGuidId)))))

            case GetContext() => State(s => (s, s.context))

            case GetDeltaId() => State(s => (s, s.deltaId))

            // Random values
            case GetPRInt() => State(_.random(_.int))
            case GetPRIntUntil(bound) => State(_.random(_.intUntil(bound)))
            case GetPRLong() => State(_.random(_.long))
            case GetPRBoolean() => State(_.random(_.boolean))
            case GetPRFloat() => State(_.random(_.float))
            case GetPRDouble() => State(_.random(_.double))


            case Put(create) => State(s => {
              // Id and revision for the new data we will create
              val id = Id[A](Guid(s.deltaId.clientId, s.deltaId.clientDeltaId, WithinDeltaId(s.currentGuidId)))
              val revision = Guid(s.deltaId.clientId, s.deltaId.clientDeltaId, WithinDeltaId(s.currentGuidId + 1))

              // Make the data item
              val createDIO: DeltaIO[U, A] = create(id)
              // Remember to increment guid twice for the guid and revision we generated
              val stateWithA: (StateData[U], A) = createDIO.foldMap(pureCompiler).run(s.withNextGuidId.withNextGuidId).value

              // We know that Put requires A <: U,
              // so the instanceOfs are safe
              val newAddedRefs = AddedRef[U](id.asInstanceOf[Id[U]], stateWithA._2.asInstanceOf[U], revision) :: stateWithA._1.addedRefs
              (stateWithA._1.copy(addedRefs = newAddedRefs), stateWithA._2)
            })
          }
      }

  }

  def runDeltaIO[U, A](dc: DeltaIO[U, A], context: DeltaIOContext, deltaId: DeltaId): DeltaRunResult[U, A] = {
    val s = dc.foldMap(new Compiler[U].pureCompiler).run(StateData.initial(context, deltaId)).value
    DeltaRunResult(s._2, s._1.addedRefs)
  }


  implicit class DeltaRun[U, A](d: Delta[U, A]) {
    def runWith(a: A, context: DeltaIOContext, deltaId: DeltaId): DeltaRunResult[U, A] = runDeltaIO(d(a), context, deltaId)
  }

  implicit class DeltaIORun[U, A](dio: DeltaIO[U, A]) {
    def runWith(context: DeltaIOContext, deltaId: DeltaId): DeltaRunResult[U, A] = runDeltaIO(dio, context, deltaId)
  }

  implicit class DeltaAndIdRun[U, A, D <: Delta[U, A]](deltaAndId: DeltaAndId[U, A, D]) {
    def runWith(a: A, context: DeltaIOContext): DeltaRunResult[U, A] = runDeltaIO(deltaAndId.delta.apply(a), context, deltaAndId.id)
  }

  implicit class DeltaWithICRun[U, A, D <: Delta[U, A]](dic: DeltaWithIC[U, A, D]) {
    def runWith(a: A): DeltaRunResult[U, A] = runDeltaIO(dic.delta.apply(a), dic.context, dic.id)
    def runWithNewContext(a: A, context: DeltaIOContext): DeltaRunResult[U, A] = runDeltaIO(dic.delta.apply(a), context, dic.id)
  }
}
