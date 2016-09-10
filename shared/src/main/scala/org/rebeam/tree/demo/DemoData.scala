package org.rebeam.tree.demo

import org.rebeam.tree._
import monocle.macros.Lenses
import upickle.default._

import scala.language.higherKinds

@Lenses case class Street(name: String, number: Int)
@Lenses case class Address(street: Street)
@Lenses case class Company(address: Address)
@Lenses case class Employee(name: String, company: Company)

@Lenses case class Todo (
  id: Int,
  name: String,
  dateCreated: String,
  dateCompleted: Option[String],
  priority: Int
)

@Lenses case class TodoList (
  name: String,
  email: String,
  color: String,
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


