package chapter07

import org.scalatest.{FlatSpec, Matchers}

class ParallelTest extends FlatSpec with Matchers {
  "Summing in parallel" should "still work" in {
    parallel.sum(Vector(1, 2, 3, 4, 5)) shouldBe 15
  }
}
