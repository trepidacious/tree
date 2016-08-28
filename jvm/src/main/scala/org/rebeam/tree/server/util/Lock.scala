package org.rebeam.tree.server.util

import java.util.concurrent.locks.ReentrantLock

class Lock {
  private val lock: ReentrantLock = new ReentrantLock()
  def apply[T](w: =>T): T = run(w)
  def run[T](w: =>T): T = {
    lock.lock()
    try {
      return w
    } finally {
      lock.unlock()
    }
  }
}

object Lock {
  def apply() = new Lock()
}
