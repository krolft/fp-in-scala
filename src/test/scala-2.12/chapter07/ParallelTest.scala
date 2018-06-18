package chapter07

import java.util.concurrent.{Executors, TimeUnit, TimeoutException}

import chapter07.Par._
import org.scalatest.{FlatSpec, Matchers}

class ParallelTest extends FlatSpec with Matchers {
  "Fold left and fold right" should "join lists as expected" in {
    // fold left is starting from left
    List(1, 2, 3).foldLeft(List[Int]())((acc, x) => x :: acc) shouldBe List(3, 2, 1)
    List(1, 2, 3).foldLeft(List[Int]())((acc, x) => acc :+ x) shouldBe List(1, 2, 3)

    // fold right is starting from right
    List(1, 2, 3).foldRight(List[Int]())((x, acc) => x :: acc) shouldBe List(1, 2, 3)
  }

  "Calling methods with a execution context" should "break for fixed thread pool of 1" in {
    val a = lazyUnit(42 + 1)
    intercept[TimeoutException] {
      Par.equal(Executors.newFixedThreadPool(1))(a, fork(a))
    }

  }
  it should "work with a non fixed thread pool with moderate thread count" in {
    val a = lazyUnit(42 + 1)
    Par.equal(Executors.newCachedThreadPool())(a, fork(a)) shouldBe true

    /*
    calculating the number of forks

    list with 2^4 elements

    16 => 2
    8 8 => 4 + 2
    4 4 4 4 => 8 + 6
    2 2 2 2 2 2 2 2 => 16 + 14 = 30
    1 1 1 1 1 1 1 1 1 1 => 0 + 30

    2 + 4 + 8 + ... + 2^(depth - 1) = 2^depth = numberOfElementsInList
     */
    Par.run(Executors.newFixedThreadPool(13))(sum(1 to math.pow(2, 4).toInt))
      .get(100, TimeUnit.MILLISECONDS) shouldBe 136
    Par.run(Executors.newCachedThreadPool)(sum(1 to math.pow(2, 10).toInt))
      .get(100, TimeUnit.MILLISECONDS) shouldBe 524800

    Par.run(Executors.newFixedThreadPool(2))(Par.parFilter((1 to 1000).toList)(_ % 2 == 0)).get(1, TimeUnit.SECONDS)
  }
}
