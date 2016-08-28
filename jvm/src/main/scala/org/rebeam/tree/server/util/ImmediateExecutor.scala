package org.rebeam.tree.server.util

import java.util.concurrent.Executor

object ImmediateExecutor extends Executor {
  override def execute(r: Runnable) = r.run()
}