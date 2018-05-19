package chapter04

import org.scalatest.{FlatSpec, Matchers}

class ValidationTest extends FlatSpec with Matchers {
  "Mapping a Validation" should "return a Failure with..." ignore {
    val failure: Validation[String, Int] = Failure("first error")
    failure.map(_ => Failure("second error")) shouldBe Failure(List("first error", "second error"))
  }
}
