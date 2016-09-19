package org.rebeam.tree.view

import japgolly.scalajs.react._
import org.rebeam.tree.Delta

import io.circe._

object SimpleRootComponent {

  case class Props[R](render: Cursor[R] => ReactElement)
  case class State[R](model: R)

  class Backend[R](scope: BackendScope[Props[R], State[R]])(implicit decoder: Decoder[R]) {

    //Apply the delta, no use for deltaJs
    val deltaToCallback = (delta: Delta[R], deltaJs: Json) => scope.modState(s => s.copy(model = delta.apply(s.model)))

    val rootParent = RootParent(deltaToCallback)

    def render(props: Props[R], state: State[R]) = {
      val rootCursor = Cursor(rootParent, state.model)
      props.render(rootCursor)
    }
  }

  def apply[R](initialModel: R)(render: Cursor[R] => ReactElement)(implicit decoder: Decoder[R]) =
    ctor(initialModel)(decoder)(Props[R](render))

  //Just make the component constructor - props to be supplied later to make a component
  def ctor[R](initialModel: R)(implicit decoder: Decoder[R]) = ReactComponentB[Props[R]]("TreeRootComponent")
    .initialState(State(initialModel))
    .backend(new Backend[R](_)(decoder))
    .render(s => s.backend.render(s.props, s.state))
    .build

}
