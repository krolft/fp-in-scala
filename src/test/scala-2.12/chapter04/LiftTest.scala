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
}
