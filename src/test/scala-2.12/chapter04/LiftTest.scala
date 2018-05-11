package chapter04

import org.scalatest.{FlatSpec, Matchers}

class LiftTest extends FlatSpec with Matchers {

  "Lifting Math.abs" should "work with optional" in {
    Chapter04.absOpt(Some(-4)) shouldBe Some(4)
    Chapter04.absOpt(Some(1)) shouldBe Some(1)
    Chapter04.absOpt(None) shouldBe None
  }

  "Try(ing) division by zero" should "result in a None" in {
    Chapter04.Try(1 / 0) shouldBe None
  }

  "Mapping two options" should "work with two Somes" in {
    Chapter04.map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
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
