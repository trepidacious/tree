package org.rebeam.tree.sync

import org.rebeam.tree.ref.Mirror
import org.rebeam.tree.sync.DeltaIORun.{AddedRef, DeltaRunResult}

trait RefAdder[U, A] {
  def addRefs(deltaRunResult: DeltaRunResult[U, A]): A
}

object RefAdder {

  private def addRefsToMirror[A](mirror: Mirror[A], addedRefs: List[AddedRef[A]]) = {
    addedRefs.foldLeft(mirror){
      case (m, ar) => m.updated(ar.id, ar.data, ar.revision)
    }
  }

  implicit def mirrorRefAdder[U]: RefAdder[U, Mirror[U]] = new RefAdder[U, Mirror[U]] {
    override def addRefs(deltaRunResult: DeltaRunResult[U, Mirror[U]]): Mirror[U] =
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
  def noOpRefAdder[U, A]: RefAdder[U, A] = new RefAdder[U, A] {
    override def addRefs(deltaRunResult: DeltaRunResult[U, A]): A = deltaRunResult.data
  }
}
