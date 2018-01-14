package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import io.circe._
import monocle.{Lens, Prism}
import org.rebeam.tree.ref.{Mirror, MirrorCodec}
import org.rebeam.tree.sync.{Identified, Ref}

/**
  * Interface provided by a parent component (view) to a child component,
  * allowing it to convert a delta into a callback that will "run" that delta.
  * This is all the child component needs to know about a parent component, in order
  * to build a tree of components that can run deltas on the root model.
  * The child's model value itself (of type C) will be passed separately, e.g.
  * using a Cursor.
  *
  * @tparam C The type of child component in the parent/child relationship.
  */
trait Parent[C] {
  def callback(delta: Delta[C]): Callback
}

/**
  * This is a parent for a view of the root of a model. Technically the root of a model
  * has no actual parent component, so this allows for deltas to be passed to a function
  * deltaToCallback to take the appropriate action. This would often be something like
  * calling modState using the delta, and perhaps sending the delta as Json to a server to
  * allow changes to be propagated to other clients, etc.
  *
  * Since other parents like LensParent require a parent themselves, this allows us to
  * start the tree of parents for a tree of views.
  *
  * @param deltaToCallback  Returns a callback handling the delta as appropriate for
  *                         the entire model.
  * @tparam R The type of the root of the model
  */
case class RootParent[R](deltaToCallback: Delta[R] => Callback) extends Parent[R] {
  def callback(delta: Delta[R]): Callback = deltaToCallback(delta)
}

/**
  * We have some model p: P, which has a child c: C that can be reached using a
  * Lens[P, C]. Given a parent of p we can use this class to produce a parent of c.
  *
  * So for example P might be Person, and C could then be String - the type of the
  * first name of that person. The fieldName would be "firstName", and lens is a
  * lens from a person to their first name. Given a Parent[Person] we wish to
  * produce a Parent[String] suitable for a view of the person's first name.
  *
  * We use a typeclass to encode the Json for the parent delta, given the child delta,
  * allowing us to work with general Lens classes (e.g. we can support encoding an
  * index, a field name or a key in a map).
  *
  * @param parent The parent of the parent component
  * @param lens The lens from the parent model to the child model
  * @tparam P The type of the parent model
  * @tparam C The type of child model
  */
case class LensParent[P, C](parent: Parent[P], lens: Lens[P, C]) extends Parent[C] {
  def callback(delta: Delta[C]): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = LensDelta(lens, delta)

    //Run using the parent's own parent
    parent.callback(parentDelta)
  }
}

case class OptionalIParent[C](parent: Parent[List[C]], optionalI: OptionalI[C]) extends Parent[C] {
  def callback(delta: Delta[C]): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = OptionalIDelta[C](optionalI, delta)

    //Run using the parent's own parent
    parent.callback(parentDelta)
  }
}

case class PrismParent[S, C](parent: Parent[S], prism: Prism[S, C]) extends Parent[C] {
  def callback(delta: Delta[C]): Callback = {
    //Produce a PrismNDelta from the provided child delta, to make it into a delta
    //of the parent (the sum class)
    val parentDelta = PrismDelta[S, C](prism, delta)

    //Run using the parent's own parent
    parent.callback(parentDelta)
  }
}


case class OptionalMatchParent[A, F <: A => Boolean](parent: Parent[List[A]], optionalMatch: OptionalMatch[A, F])(implicit cEncoder: Encoder[F]) extends Parent[A] {
  def callback(delta: Delta[A]): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = OptionalMatchDelta[A, F](optionalMatch, delta)

    //Run using the parent's own parent
    parent.callback(parentDelta)
  }
}

case class OptionalIdParent[A <: Identified[A]](parent: Parent[List[A]], optionalId: OptionalId[A]) extends Parent[A] {
  def callback(delta: Delta[A]): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = OptionalIdDelta[A](optionalId, delta)

    //Run using the parent's own parent
    parent.callback(parentDelta)
  }
}

case class OptionalRefParent[A](parent: Parent[List[Ref[A]]], optionalRef: OptionalRef[A]) extends Parent[Ref[A]] {
  def callback(delta: Delta[Ref[A]]): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = OptionalRefDelta[A](optionalRef, delta)

    //Run using the parent's own parent
    parent.callback(parentDelta)
  }
}

case class OptionParent[C](parent: Parent[Option[C]]) extends Parent[C] {
  def callback(delta: Delta[C]): Callback = {
    //Produce an OptionDelta from the provided child delta, to make it into a delta
    //of the parent (i.e. convert child's Delta[C] to parent's Delta[Option[C]]
    val parentDelta = OptionDelta[C](delta)

    //Run using the parent's own parent
    parent.callback(parentDelta)
  }
}


/**
  * Produce a Parent for a data item in a Mirror, from a Parent for that Mirror. This uses
  * an Ref to move between the Mirror and a data item
  * @param parent         The parent of the Mirror
  * @param ref            The Ref to use to look up data in Mirror
  * @tparam A             The type of data item in the Mirror
  */
case class MirrorParent[A: MirrorCodec](parent: Parent[Mirror], ref: Ref[A]) extends Parent[A] {

  def callback(delta: Delta[A]): Callback = {
    //Produce a MirrorDelta from the provided child delta, to make it into a delta
    //of the parent (i.e. convert child's Delta[A] to parent's Delta[Mirror]
    val parentDelta = MirrorDelta(ref, delta)

    //Run using the parent's own parent
    parent.callback(parentDelta)
  }
}



