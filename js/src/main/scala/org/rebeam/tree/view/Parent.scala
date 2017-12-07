package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._

/**
  * Interface provided by a parent component (view) to a child component,
  * allowing it to convert a delta (and JSON representation of that delta) into
  * a callback that will "run" that delta).
  * This is all the child component needs to know about a parent component, in order
  * to build a tree of components that can run deltas on the root model.
  * The child's model value itself (of type C) will be passed separately, e.g.
  * using a Cursor.
  *
  * @tparam C The type of child model in the parent/child relationship.
  * @tparam D The type of delta on the child model
  */
trait Parent[C, D <: Delta[C]] {
  def callback(delta: D): Callback
}

/**
  * This is a parent for a view of the root of a model. Technically the root of a model
  * has no actual parent component, so this allows for deltas to be passed to a function
  * deltaToCallback to take the appropriate action. This would often be something like
  * calling modState using the delta, and perhaps sending the deltaJs to a server to
  * allow changes to be propagated to other clients, etc.
  *
  * Since other parents require a parent themselves, this allows us to
  * start the tree of parents for a tree of views.
  *
  * @param deltaToCallback  Returns a callback handling the delta as appropriate for the entire model.
  * @tparam R The type of the root of the model
  * @tparam D The type of delta on the root model
  */
case class RootParent[R, D <: Delta[R]](deltaToCallback: D => Callback) extends Parent[R, D] {
  def callback(delta: D): Callback = deltaToCallback(delta)
}

/**
  * Given a parent with a model: M and delta: D, produces a parent
  * for a child component with model: C and delta: E. This works by
  * just transforming the child delta: E to parent delta: D using a
  * provided function (often based on a constructor), and then using
  * the provided parent to produce the callback.
  * This allows us to chain Parents while navigating through a data
  * model.
  * @param parent               The parent to use to produce callbacks
  * @param childToParentDelta   Transforms from child to parent deltas
  * @tparam M                   The type of model in the parent
  * @tparam D                   The type of delta on the parent model
  * @tparam C                   The type of model in the child
  * @tparam E                   The type of delta on the child model
  */
case class ChildParent[M, D <: Delta[M], C, E <: Delta[C]](parent: Parent[M, D], childToParentDelta: E => D) extends Parent[C, E] {
  def callback(delta: E): Callback = parent.callback(childToParentDelta(delta))
}
