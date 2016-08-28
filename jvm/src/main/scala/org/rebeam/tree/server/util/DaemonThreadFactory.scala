package org.rebeam.tree.server.util

import java.util.concurrent.{Executors, ThreadFactory}

class DaemonThreadFactory extends ThreadFactory {
  val del = Executors.defaultThreadFactory()
  override def newThread(r: Runnable) = {
    val t = del.newThread(r)
    t.setDaemon(true)
    t
  }
}

object DaemonThreadFactory {
  def apply() = new DaemonThreadFactory()
}
