package org.rebeam.tree.demo

import org.rebeam.tree._
import monocle.macros.Lenses
import org.rebeam.tree.view.Color

import io.circe._
import io.circe.generic.semiauto._

import scala.language.higherKinds

import BasicDeltaDecoders._
import DeltaDecoder._

object DemoData {

  @Lenses case class Street(name: String, number: Int)
  @Lenses case class Address(street: Street)
  @Lenses case class Company(address: Address)
  @Lenses case class Employee(name: String, company: Company)

  sealed trait StreetAction extends Delta[Street]

  object StreetAction {
    case class NumberMultiple(multiple: Int) extends StreetAction {
      def apply(s: Street): Street = s.copy(number = s.name.length * multiple)
    }

    case object Capitalise extends StreetAction {
      def apply(s: Street): Street = s.copy(name = s.name.toLowerCase.capitalize)
    }
  }

  implicit val streetDecoder: Decoder[Street] = deriveDecoder[Street]
  implicit val streetEncoder: Encoder[Street] = deriveEncoder[Street]

  implicit val addressDecoder: Decoder[Address] = deriveDecoder[Address]
  implicit val addressEncoder: Encoder[Address] = deriveEncoder[Address]

  implicit val companyDecoder: Decoder[Company] = deriveDecoder[Company]
  implicit val companyEncoder: Encoder[Company] = deriveEncoder[Company]

  implicit val employeeDecoder: Decoder[Employee] = deriveDecoder[Employee]
  implicit val employeeEncoder: Encoder[Employee] = deriveEncoder[Employee]

  implicit val streetActionDecoder: Decoder[StreetAction] = deriveDecoder[StreetAction]
  implicit val streetActionEncoder: Encoder[StreetAction] = deriveEncoder[StreetAction]

  implicit val streetDeltaDecoder =
    value[Street] or lens("name", Street.name) or lens("number", Street.number) or action[Street, StreetAction]

  implicit val addressDeltaDecoder = value[Address] or lens("street", Address.street)


  sealed trait Priority
  object Priority {
    object Low extends Priority
    object Medium extends Priority
    object High extends Priority
  }

  @Lenses case class Todo (
                            id: Int,
                            name: String,
                            created: Moment,
                            completed: Option[Moment] = None,
                            priority: Priority = Priority.Medium
                          )

  sealed trait TodoAction extends Delta[Todo]
  object TodoAction {
    case class Complete(completed: Moment) extends TodoAction {
      def apply(t: Todo): Todo = t.copy(completed = Some(completed))
    }
  }

  @Lenses case class TodoList (
                                name: String,
                                email: String,
                                color: Color,
                                items: List[Todo],
                                nextId: Int = 1
                              )

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

  implicit val priorityDecoder: Decoder[Priority] = deriveDecoder[Priority]
  implicit val priorityEncoder: Encoder[Priority] = deriveEncoder[Priority]
  implicit val priorityDeltaDecoder = value[Priority]

  implicit val colorDecoder: Decoder[Color] = deriveDecoder[Color]
  implicit val colorEncoder: Encoder[Color] = deriveEncoder[Color]
  implicit val colorDeltaDecoder = value[Color]

  implicit val momentDecoder: Decoder[Moment] = deriveDecoder[Moment]
  implicit val momentEncoder: Encoder[Moment] = deriveEncoder[Moment]

  implicit val todoDecoder: Decoder[Todo] = deriveDecoder[Todo]
  implicit val todoEncoder: Encoder[Todo] = deriveEncoder[Todo]

  implicit val todoActionDecoder: Decoder[TodoAction] = deriveDecoder[TodoAction]
  implicit val todoActionEncoder: Encoder[TodoAction] = deriveEncoder[TodoAction]

  implicit val todoDeltaDecoder = value[Todo] or lens("name", Todo.name) or lens("priority", Todo.priority) or action[Todo, TodoAction]

  implicit val todoListDecoder: Decoder[TodoList] = deriveDecoder[TodoList]
  implicit val todoListEncoder: Encoder[TodoList] = deriveEncoder[TodoList]

  implicit val todoListActionDecoder: Decoder[TodoListAction] = deriveDecoder[TodoListAction]
  implicit val todoListActionEncoder: Encoder[TodoListAction] = deriveEncoder[TodoListAction]

  implicit val todoListDeltaDecoder =
      value[TodoList] or lens("name", TodoList.name) or lens("email", TodoList.email) or lens("color", TodoList.color) or action[TodoList, TodoListAction]

}

