package chapter04

import org.scalatest.{FlatSpec, Matchers}

class Chapter04Test extends FlatSpec with Matchers {
  "Calculating the variance" should "return None for an empty sequence" in {
    Chapter04.variance(List()) shouldBe None
  }

  it should "work for sequences with one element" in  {
    Chapter04.variance(List(2)) shouldBe Some(0)
  }

  it should "work for 1, 3, 5)" in {
    Chapter04.variance(List(1, 3, 5)) shouldBe Some(8.0 / 3)
  }
}