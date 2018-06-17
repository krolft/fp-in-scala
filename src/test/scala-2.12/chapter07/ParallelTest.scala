package chapter07

import org.scalatest.{FlatSpec, Matchers}

class Parallel2Test extends FlatSpec with Matchers {
  "Fold left and fold right" should "join lists as expected" in {
    // fold left is starting from left
    List(1, 2, 3).foldLeft(List[Int]())((acc, x) => x :: acc) shouldBe List(3, 2, 1)
    List(1, 2, 3).foldLeft(List[Int]())((acc, x) => acc :+ x) shouldBe List(1, 2, 3)

    // fold right is starting from right
    List(1, 2, 3).foldRight(List[Int]())((x, acc) => x :: acc) shouldBe List(1, 2, 3)
  }
}
