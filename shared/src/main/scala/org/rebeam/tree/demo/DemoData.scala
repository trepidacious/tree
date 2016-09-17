package org.rebeam.tree.demo

import org.rebeam.tree._
import monocle.macros.Lenses
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

@Lenses case class TodoList (
  name: String,
  email: String,
  color: Color,
  items: List[Todo]
)

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


