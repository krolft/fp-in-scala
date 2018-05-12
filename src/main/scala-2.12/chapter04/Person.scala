package chapter04

case class Person(name: Name, age: Age)

sealed case class Name(value: String)

sealed case class Age(value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == null || name.trim.isEmpty) Left("Name is empty.")
    else Right(Name(name.trim))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).flatMap(name => mkAge(age).map(age => Person(name, age)))
}
