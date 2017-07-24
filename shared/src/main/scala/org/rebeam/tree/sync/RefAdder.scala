package org.rebeam.tree.sync

import org.rebeam.tree.ref.{Mirror, MirrorCodec}
import org.rebeam.tree.sync.DeltaIORun.{AddedRef, DeltaRunResult}

trait RefAdder[A] {
  def addRefs(deltaRunResult: DeltaRunResult[A]): A
}

object RefAdder {
  implicit val mirrorRefAdder: RefAdder[Mirror] = new RefAdder[Mirror] {
    override def addRefs(deltaRunResult: DeltaRunResult[Mirror]): Mirror = {
      deltaRunResult.addedRefs.foldLeft(deltaRunResult.data){
        case (mirror, addedRef) => {
          val ar = addedRef//.asInstanceOf[AddedRef[Any]]
          mirror.updated(ar.id, ar.data, ar.revision)(ar.codec)
        }
      }
    }
  }

  /**
    * This can be used where the data type has no support for Refs.
    * @tparam A The data type
    * @return   A RefAdder that does nothing with Refs, leaving data unaltered
    */
  def noOpRefAdder[A]: RefAdder[A] = new RefAdder[A] {
    override def addRefs(deltaRunResult: DeltaRunResult[A]): A = deltaRunResult.data
  }
}
