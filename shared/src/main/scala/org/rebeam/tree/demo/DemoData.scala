package org.rebeam.tree.demo

import org.rebeam.tree._
//This macro gets us LensN rather than just Lens
import org.rebeam.lenses.macros.Lenses
//import monocle.macros.Lenses
import org.rebeam.tree.view.Color
import io.circe._
import io.circe.generic.semiauto._

import scala.language.higherKinds
import BasicDeltaDecoders._
import DeltaCodecs._
import io.circe.generic.JsonCodec

object DemoData {

  @JsonCodec
  @Lenses
  case class Street(name: String, number: Int)

  @JsonCodec
  @Lenses
  case class Address(street: Street)

  @JsonCodec
  @Lenses
  case class Company(address: Address)

  @JsonCodec
  @Lenses
  case class Employee(name: String, company: Company)

  @JsonCodec
  sealed trait StreetAction extends Delta[Street]
  object StreetAction {
    case class NumberMultiple(multiple: Int) extends StreetAction {
      def apply(s: Street): Street = s.copy(number = s.name.length * multiple)
    }

    case object Capitalise extends StreetAction {
      def apply(s: Street): Street = s.copy(name = s.name.toLowerCase.capitalize)
    }
  }

  // Alternative to @JsonCodec
//  implicit val streetDecoder: Decoder[Street] = deriveDecoder[Street]
//  implicit val streetEncoder: Encoder[Street] = deriveEncoder[Street]

  implicit val streetDeltaDecoder =
    value[Street] or lensN(Street.name) or lensN(Street.number) or action[Street, StreetAction]

  implicit val addressDeltaDecoder = value[Address] or lensN(Address.street)

  @JsonCodec
  sealed trait Priority
  object Priority {
    object Low extends Priority
    object Medium extends Priority
    object High extends Priority
  }

  @JsonCodec
  @Lenses
  case class Todo (
                            id: Int,
                            name: String,
                            created: Moment,
                            completed: Boolean = false,
                            priority: Priority = Priority.Medium
                          )

  @JsonCodec
  sealed trait TodoAction extends Delta[Todo]
  object TodoAction {
//    case class Complete(completed: Moment) extends TodoAction {
//      def apply(t: Todo): Todo = t.copy(completed = Some(completed))
//    }
    case object CyclePriority extends TodoAction {
      def apply(t: Todo): Todo = t.copy(priority =
        if (t.priority == Priority.Low) {
          Priority.Medium
        } else if (t.priority == Priority.Medium) {
          Priority.High
        } else {
          Priority.Low
        }
      )
    }
  }

  @JsonCodec
  @Lenses
  case class TodoList (
                                name: String,
                                email: String,
                                color: Color,
                                items: List[Todo],
                                nextId: Int = 1
                              )

  //Works with Cursor.zoomMatch to zoom to a particular Todo
  @JsonCodec
  case class FindTodoById(id: Int) extends (Todo => Boolean) {
    def apply(t: Todo): Boolean = t.id == id
  }

  @JsonCodec
  sealed trait TodoListAction extends Delta[TodoList]
  object TodoListAction {

    case class CreateTodo(created: Moment, name: String = "New todo", priority: Priority = Priority.Medium) extends TodoListAction {
      def apply(l: TodoList): TodoList = {
        val t = Todo(l.nextId, name, created, false, priority)
        l.copy(items = t :: l.items, nextId = l.nextId + 1)
      }
    }

    case class DeleteExactTodo(t: Todo) extends TodoListAction {
      def apply(l: TodoList): TodoList = l.copy(items = l.items.filterNot(_ == t))
    }

    case class DeleteTodoById(id: Int) extends TodoListAction {
      def apply(l: TodoList): TodoList = l.copy(items = l.items.filterNot(_.id == id))
    }
  }

  //These don't have codecs in their own file
  implicit val colorDecoder: Decoder[Color] = deriveDecoder[Color]
  implicit val colorEncoder: Encoder[Color] = deriveEncoder[Color]
  implicit val momentDecoder: Decoder[Moment] = deriveDecoder[Moment]
  implicit val momentEncoder: Encoder[Moment] = deriveEncoder[Moment]

  //Delta decoders

  //These can only be replaced with a new value
  implicit val priorityDeltaDecoder = value[Priority]
  implicit val colorDeltaDecoder = value[Color]

  implicit val todoDeltaDecoder = value[Todo] or lensN(Todo.name) or lensN(Todo.priority) or lensN(Todo.completed) or action[Todo, TodoAction]

  //This makes it possible to act on any List[Todo] using an OptionalIDelta or an OptionalMatchDelta
  implicit val listOfTodoDeltaDecoder = optionalI[Todo] or optionalMatch[Todo, FindTodoById]

  implicit val todoListDeltaDecoder =
      value[TodoList] or lensN(TodoList.name) or lensN(TodoList.items) or lensN(TodoList.email) or lensN(TodoList.color) or action[TodoList, TodoListAction]

}

