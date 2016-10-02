package org.rebeam.tree.demo

import org.rebeam.tree._
import monocle.macros.Lenses
import org.rebeam.tree.view.Color
import io.circe._
import io.circe.generic.semiauto._

import scala.language.higherKinds
import BasicDeltaDecoders._
import DeltaCodecs._
import io.circe.generic.JsonCodec
import monocle.{Lens, Optional}

object DemoData {

  @JsonCodec
  @Lenses
  case class Street(name: String, number: Int)

  object Street {
    val nameN = LensN("name", Street.name)
    val numberN = LensN("number", Street.number)
  }

  @JsonCodec
  @Lenses
  case class Address(street: Street)
  object Address {
    val streetN = LensN("street", Address.street)
  }

  @JsonCodec
  @Lenses
  case class Company(address: Address)
  object Company {
    val addressN = LensN("address", Company.address)
  }

  @JsonCodec
  @Lenses
  case class Employee(name: String, company: Company)
  object Employee {
    val nameN = LensN("name", Employee.name)
    val companyN = LensN("company", Employee.company)
  }

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
    value[Street] or lensN(Street.nameN) or lensN(Street.numberN) or action[Street, StreetAction]

  implicit val addressDeltaDecoder = value[Address] or lensN(Address.streetN)

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
  object Todo {
    val idN = LensN("id", Todo.id)
    val nameN = LensN("name", Todo.name)
    val createdN = LensN("created", Todo.created)
    val completedN = LensN("completed", Todo.completed)
    val priorityN = LensN("priority", Todo.priority)
  }

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
                              ) {
    def replaceTodoById(newTodo: Todo): TodoList = copy(items = items.map(oldTodo => if (oldTodo.id == newTodo.id) newTodo else oldTodo))
    def todoById(id: Int): Option[Todo] = items.find(_.id == id)
  }
  object TodoList {
    val nameN = LensN("name", TodoList.name)
    val emailN = LensN("email", TodoList.email)
    val colorN = LensN("color", TodoList.color)
    val itemsN = LensN("items", TodoList.items)
    val nextIdN = LensN("nextId", TodoList.nextId)

    def todoById(id: Int): Optional[TodoList, Todo] = Optional[TodoList, Todo](_.todoById(id))(newTodo => list => list.replaceTodoById(newTodo))
  }

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

  implicit val todoDeltaDecoder = value[Todo] or lensN(Todo.nameN) or lensN(Todo.priorityN) or action[Todo, TodoAction]

  //This makes it possible to act on any List[Todo] using an OptionalIDelta
  implicit val listOfTodoDeltaDecoder = optionalI[Todo]

  implicit val todoListDeltaDecoder =
      value[TodoList] or lensN(TodoList.nameN) or lensN(TodoList.itemsN) or lensN(TodoList.emailN) or lensN(TodoList.colorN) or action[TodoList, TodoListAction]

}

