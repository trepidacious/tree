package org.rebeam.tree.sync

import cats.data.State
import cats.~>
import org.rebeam.tree.DeltaContext._
import org.rebeam.tree.sync.Sync._

object DeltaContextInterpreter {

  //Which delta we are in and which id we are up to
  private type DeltaContextState[A] = State[(DeltaId, Long), A]

  private val pureCompiler: DeltaContextA ~> DeltaContextState =
    new (DeltaContextA ~> DeltaContextState) {
      def apply[A](fa: DeltaContextA[A]): DeltaContextState[A] =
        fa match {
          case GetId() => State(s => ((s._1, s._2 + 1), Guid[Any](s._1.clientId, s._1.clientDeltaId, s._2)))
        }
    }

  def run[A](dc: DeltaContext[A], deltaId: DeltaId): A =
    dc.foldMap(pureCompiler).run((deltaId, 0)).value._2

}
