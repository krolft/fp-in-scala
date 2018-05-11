package chapter04

import org.scalatest.{FlatSpec, Matchers}

class Chapter04Test extends FlatSpec with Matchers {
  "Calculating the variance" should "return None for an empty sequence" in {
    Chapter04.variance(List()) shouldBe None
  }

  it should "work for sequences with one element" in  {
    Chapter04.variance(List(2)) shouldBe Some(0)
  }

  it should "work for (1, 3, 5)" in {
    Chapter04.variance(List(1, 3, 5)) shouldBe Some(8.0 / 3)
  }

  "Lifting a calculation" should "return a Right, calculated from the lifted function, for happy path" in {
    Chapter04.useForComprehensionToLift("2", "4") shouldBe Right(9.0)
    Chapter04.useMap2ToLift("2", "4") shouldBe Right(9.0)
  }

  it should "return a Left if either one or all parameters are invalid" in {
    Chapter04.useForComprehensionToLift("a", "4") shouldBe a[Left[_]]
    Chapter04.useForComprehensionToLift("2", "b") shouldBe a[Left[_]]
    Chapter04.useForComprehensionToLift("a", "b") shouldBe a[Left[_]]

    Chapter04.useMap2ToLift("a", "4") shouldBe a[Left[_]]
    Chapter04.useMap2ToLift("2", "b") shouldBe a[Left[_]]
    Chapter04.useMap2ToLift("a", "b") shouldBe a[Left[_]]
  }
}