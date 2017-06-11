package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree._
import org.rebeam.lenses._
import io.circe._
import monocle.Lens
import org.rebeam.tree.ref.Cache
import org.rebeam.tree.sync.Sync.Ref

/**
  * Interface provided by a parent component (view) to a child component,
  * allowing it to convert a delta (and JSON representation of that delta) into
  * a callback that will "run" that delta).
  * This is all the child component needs to know about a parent component, in order
  * to build a tree of components that can run deltas on the root model.
  * The child's model value itself (of type C) will be passed separately, e.g.
  * using a Cursor.
  *
  * @tparam C The type of child component in the parent/child relationship.
  */
trait Parent[C] {
  def callback(delta: Delta[C], deltaJs: Json): Callback
}

/**
  * This is a parent for a view of the root of a model. Technically the root of a model
  * has no actual parent component, so this allows for deltas to be passed to a function
  * deltaToCallback to take the appropriate action. This would often be something like
  * calling modState using the delta, and perhaps sending the deltaJs to a server to
  * allow changes to be propagated to other clients, etc.
  *
  * Since other parents like LensParent require a parent themselves, this allows us to
  * start the tree of parents for a tree of views.
  *
  * @param deltaToCallback  Returns a callback handling the delta as appropriate for the entire model.
  * @tparam R The type of the root of the model
  */
case class RootParent[R](deltaToCallback: (Delta[R], Json) => Callback) extends Parent[R] {
  def callback(delta: Delta[R], deltaJs: Json): Callback = deltaToCallback(delta, deltaJs)
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
  * @param parent The parent of the parent component
  * @param lensN The lens from the parent model to the child model
  * @tparam P The type of the parent model
  * @tparam C The type of child model
  */
case class LensNParent[P, C](parent: Parent[P], lensN: LensN[P, C]) extends Parent[C] {
  def callback(delta: Delta[C], deltaJs: Json): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = LensNDelta(lensN, delta)

    //Add this delta to the JSON
    val parentDeltaJs = Json.obj(
      "lens" -> Json.obj(
        "lensN" -> Json.obj(
          lensN.name -> deltaJs)
      )
    )

    //Run using the parent's own parent
    parent.callback(parentDelta, parentDeltaJs)
  }
}

case class OptionalIParent[C](parent: Parent[List[C]], optionalI: OptionalI[C]) extends Parent[C] {
  def callback(delta: Delta[C], deltaJs: Json): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = OptionalIDelta[C](optionalI, delta)

    //Add this delta to the JSON
    val parentDeltaJs = Json.obj(
      "optional" -> Json.obj(
        "optionalI" -> Json.obj(
          "index" -> Json.fromInt(optionalI.index),
          "delta" -> deltaJs
        )
      )
    )

    //Run using the parent's own parent
    parent.callback(parentDelta, parentDeltaJs)
  }
}

case class PrismNParent[S, C](parent: Parent[S], prismN: PrismN[S, C]) extends Parent[C] {
  def callback(delta: Delta[C], deltaJs: Json): Callback = {
    //Produce a PrismNDelta from the provided child delta, to make it into a delta
    //of the parent (the sum class)
    val parentDelta = PrismNDelta[S, C](prismN, delta)

    //Add this delta to the JSON
    val parentDeltaJs = Json.obj(
      "prism" -> Json.obj(
        "prismN" -> Json.obj(
          prismN.name -> deltaJs
        )
      )
    )

    //Run using the parent's own parent
    parent.callback(parentDelta, parentDeltaJs)
  }
}


case class OptionalMatchParent[A, F <: A => Boolean](parent: Parent[List[A]], optionalMatch: OptionalMatch[A, F])(implicit cEncoder: Encoder[F]) extends Parent[A] {
  def callback(delta: Delta[A], deltaJs: Json): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = OptionalMatchDelta[A, F](optionalMatch, delta)

    //Add this delta to the JSON
    val parentDeltaJs = Json.obj(
      "optional" -> Json.obj(
        "optionalMatch" -> Json.obj(
          "find" -> cEncoder(optionalMatch.f),
          "delta" -> deltaJs
        )
      )
    )

    //Run using the parent's own parent
    parent.callback(parentDelta, parentDeltaJs)
  }
}

case class OptionParent[C](parent: Parent[Option[C]]) extends Parent[C] {
  def callback(delta: Delta[C], deltaJs: Json): Callback = {
    //Produce an OptionDelta from the provided child delta, to make it into a delta
    //of the parent (i.e. convert child's Delta[C] to parent's Delta[Option[C]]
    val parentDelta = OptionDelta[C](delta)

    //Add this delta to the JSON
    val parentDeltaJs = Json.obj(
      "option" -> Json.obj(
        "delta" -> deltaJs
      )
    )

    //Run using the parent's own parent
    parent.callback(parentDelta, parentDeltaJs)
  }
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
case class LensParent[P, C, L <: Lens[P, C] : OuterEncoder](parent: Parent[P], lens: L) extends Parent[C] {
  def callback(delta: Delta[C], deltaJs: Json): Callback = {
    //Produce a LensDelta from the provided child delta, to make it into a delta
    //of the parent
    val parentDelta = LensDelta(lens, delta)

    //Add this delta to the JSON
    val parentDeltaJs = Json.obj("lens" -> implicitly[OuterEncoder[L]].encode(lens, deltaJs))

    //Run using the parent's own parent
    parent.callback(parentDelta, parentDeltaJs)
  }
}

/**
  * Produce a Parent for a data item in a Cache, from a Parent for that Cache. This uses
  * an OptionalCache to move between the Cache and the data item at a given Ref.
  * @param parent         The parent of the cache
  * @param optionalCache  The Optional from the Cache to the correct data item (if any)
  * @tparam M             The type of data item in the Cache
  * @tparam A             The type of the data item
  */
case class CacheParent[M, A <: M](parent: Parent[Cache[M]], optionalCache: OptionalCache[M, A]) extends Parent[A] {

  def callback(delta: Delta[A], deltaJs: Json): Callback = {
    //Produce an OptionalCacheDelta from the provided child delta, to make it into a delta
    //of the parent (i.e. convert child's Delta[A] to parent's Delta[Cache[M]
    val parentDelta = CacheDelta(optionalCache, delta)

    //Add this delta to the JSON
    val parentDeltaJs = Json.obj(
      "optional" -> Json.obj(
        "optionalCache" -> Json.obj(
          "ref" -> Ref.encodeRef[A](optionalCache.ref),
          "delta" -> deltaJs
        )
      )
    )

    //Run using the parent's own parent
    parent.callback(parentDelta, parentDeltaJs)
  }
}



