package org.rebeam.tree

import japgolly.scalajs.react._
import upickle.Js

object TreeRootComponent {

  case class Props[R](render: Cursor[R] => ReactElement)

  class Backend[R](scope: BackendScope[Props[R], R]) {

    //Apply the delta, and print its Json. In a real implementation this
    //would still apply the delta, but would also send the Json to a server
    //to attempt to "commit" the change. The state might actually store a
    //tentative Address as modified locally, and a last-known authoritative
    //Address from the server, to allow reverting local modifications if they
    //are not confirmed, or merging them if the server reports it merged them.
    val deltaToCallback = (delta: Delta[R], deltaJs: Js.Value) =>
      scope.modState(delta.apply) >> Callback(println("Delta >> " + deltaJs.toString))

    val rootParent = RootParent(deltaToCallback)

    def render(props: Props[R], r: R) = {
      val rootCursor = Cursor(rootParent, r)
      props.render(rootCursor)
    }
  }

  //Make the component itself, by providing a render method to initialise the props
  def apply[R](initialModel: R)(render: Cursor[R] => ReactElement) = ctor(initialModel)(TreeRootComponent.Props[R](render))

  //Just make the component constructor - props to be supplied later to make a component
  def ctor[R](initialModel: R) = ReactComponentB[Props[R]]("AddressView")
    .initialState(initialModel)
    .backend(new Backend[R](_))
    .render(s => s.backend.render(s.props, s.state))
    .build

}
