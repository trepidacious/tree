package org.rebeam.tree.demo

import org.rebeam.tree._
import monocle.macros.Lenses
import org.rebeam.tree.DeltaReaders._
import org.rebeam.tree.view.Color
import upickle.default._

import scala.language.higherKinds

@Lenses case class Street(name: String, number: Int)
@Lenses case class Address(street: Street)
@Lenses case class Company(address: Address)
@Lenses case class Employee(name: String, company: Company)

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
  completed: Option[Moment],
  priority: Priority
)

sealed trait TodoAction extends Delta[Todo]
object TodoAction {
  case class Complete(completed: Moment) extends TodoAction {
    def apply(t: Todo): Todo = t.copy(completed = Some(completed))
  }
}

object Todo {
  import BasicDeltaReaders._
  implicit val priorityDeltaReader = deltaReaderFromReader[Priority]

  implicit val todoDeltaReader =
    DeltaReader.build[Todo]
      .lens("name", Todo.name)
      .lens("priority", Todo.priority)
      .action[TodoAction]
}

@Lenses case class TodoList (
  name: String,
  email: String,
  color: Color,
  items: List[Todo],
  nextId: Int
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

object TodoList {
  import BasicDeltaReaders._
  implicit val colorDeltaReader = deltaReaderFromReader[Color]

  implicit val dr =
    DeltaReader.build[TodoList]
      .lens("name", TodoList.name)
      .lens("email", TodoList.email)
      .lens("color", TodoList.color)
      .action[TodoListAction]
}

sealed trait StreetAction extends Delta[Street]

object StreetAction {
  case class NumberMultiple(multiple: Int) extends StreetAction {
    def apply(s: Street): Street = s.copy(number = s.name.length * multiple)
  }

  case object Capitalise extends StreetAction {
    def apply(s: Street): Street = s.copy(name = s.name.toLowerCase.capitalize)
  }
}

object Street {
  import BasicDeltaReaders._
  implicit val streetDeltaReader =
    DeltaReader.build[Street]
      .lens("name", Street.name)
      .lens("number", Street.number)
      .action[StreetAction]
}

object Address {
  import Street._

  implicit val addressDeltaReader =
    DeltaReader.build[Address]
      .lens("street", Address.street)
}

object Company {
}

object Employee {
}


