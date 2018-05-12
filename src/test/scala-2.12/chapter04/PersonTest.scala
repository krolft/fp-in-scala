package chapter04

import org.scalatest.{FlatSpec, Matchers}

class PersonTest extends FlatSpec with Matchers {
  "Making a Person" should "return a Left if the name is invalid" in {
    Person.mkPerson("", 1) shouldBe Left("Name is empty.")
  }

  it should "return a Left if the age is invalid" in {
    Person.mkPerson("Hans", -1) shouldBe Left("Age is out of range.")
  }

  it should "return a Left specific to the name if name and age are invalid" in {
    Person.mkPerson(null, -1) shouldBe Left("Name is empty.")
  }

  it should "return a Person object if both name and age are valid" in {
    Person.mkPerson("Hans", 1) shouldBe Right(Person(Name("Hans"), Age(1)))
  }
}
