package org.rebeam.tree.demo

import org.rebeam.tree._
import monocle.macros.Lenses
import org.rebeam.tree.view.Color
import io.circe._
import io.circe.generic.semiauto._

import scala.language.higherKinds
import BasicDeltaDecoders._
import DeltaDecoder._
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
    value[Street] or lens("name", Street.name) or lens("number", Street.number) or action[Street, StreetAction]

  implicit val addressDeltaDecoder = value[Address] or lens("street", Address.street)

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
                            completed: Option[Moment] = None,
                            priority: Priority = Priority.Medium
                          )

  @JsonCodec
  sealed trait TodoAction extends Delta[Todo]
  object TodoAction {
    case class Complete(completed: Moment) extends TodoAction {
      def apply(t: Todo): Todo = t.copy(completed = Some(completed))
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

  @JsonCodec
  sealed trait TodoListAction extends Delta[TodoList]
  object TodoListAction {

    case class CreateTodo(created: Moment, name: String = "New todo", priority: Priority = Priority.Medium) extends TodoListAction {
      def apply(l: TodoList): TodoList = {
        val t = Todo(l.nextId, name, created, None, priority)
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

  //More complex deltas - can replace entire value, or operate using lenses or actions
  implicit val todoDeltaDecoder = value[Todo] or lens("name", Todo.name) or lens("priority", Todo.priority) or action[Todo, TodoAction]
  implicit val todoListDeltaDecoder =
      value[TodoList] or lens("name", TodoList.name) or lens("email", TodoList.email) or lens("color", TodoList.color) or action[TodoList, TodoListAction]

}

