package org.rebeam.tree.sync

import org.rebeam.tree.Delta
import org.rebeam.tree.ref.Mirror
import org.rebeam.tree.sync.DeltaIORun.{AddedRef, DeltaRunResult}

trait RefAdder[U, A] {
  def addRefs(deltaRunResult: DeltaRunResult[U, A]): A
}

object RefAdder {

  private def addRefsToMirror[U, A, D <: Delta[U, A]](mirror: Mirror[U, A, D], addedRefs: List[AddedRef[U]]) = {
    addedRefs.foldLeft(mirror){
      case (m, addedRef) => {
        val ar = addedRef
        m.updated(ar.id, ar.data, ar.revision)(ar.codec)
      }
    }
  }

  implicit val mirrorRefAdder: RefAdder[Mirror] = new RefAdder[Mirror] {
    override def addRefs(deltaRunResult: DeltaRunResult[Mirror]): Mirror =
      addRefsToMirror(deltaRunResult.data, deltaRunResult.addedRefs)
  }

  //FIXME MIRRORANDID
//  implicit def mirrorAndIdRefAdder[A]: RefAdder[MirrorAndId[A]] = new RefAdder[MirrorAndId[A]] {
//    override def addRefs(deltaRunResult: DeltaRunResult[MirrorAndId[A]]): MirrorAndId[A] = {
//      val newMirror = addRefsToMirror(deltaRunResult.data.mirror, deltaRunResult.addedRefs)
//      deltaRunResult.data.copy(mirror = newMirror)
//    }
//  }

  /**
    * This can be used where the data type has no support for Refs.
    * @tparam A The data type
    * @return   A RefAdder that does nothing with Refs, leaving data unaltered
    */
  def noOpRefAdder[A]: RefAdder[A] = new RefAdder[A] {
    override def addRefs(deltaRunResult: DeltaRunResult[A]): A = deltaRunResult.data
  }
}
