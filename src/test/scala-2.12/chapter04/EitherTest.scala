package chapter04

import java.lang
import java.lang.ArithmeticException

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {
  "Mapping over an Either" should "return the existing Left" in {
    val left: Either[String, Int] = Left("some error")
    left.map(_ + 1) shouldBe Left("some error")
  }

  it should "return a new Right when called on a Right" in {
    val right: Either[String, Int] = Right(1)
    right.map(_ + 1) shouldBe Right(2)
  }

  "Flat mapping over an Either" should "return the existing Left" in {
    val left: Either[String, Int] = Left("old error")
    left.flatMap(_ => Left("new error")) shouldBe Left("old error")
    left.flatMap(v => Right(v + 1)) shouldBe Left("old error")
  }

  it should "return a new Right when called on a Right" in {
    val right: Either[String, Int] = Right(1)
    right.flatMap(v => Right(v + 1)) shouldBe Right(2)
  }

  "Or-else-ing over an Either" should "use the passed Either if called on a Left" in {
    val left: Either[String, Int] = Left("old error")
    left.orElse(Left("new error")) shouldBe Left("new error")
    left.orElse(Right(2)) shouldBe Right(2)
  }

  it should "return itself if called on a Right" in {
    val right: Either[String, Int] = Right(1)
    right.orElse(Left("new error")) shouldBe Right(1)
    right.orElse(Right(2)) shouldBe Right(1)
  }

  "Map2-ing over an Either" should "return a Left if one of the Either-s is left" in {
    val left: Either[String, Int] = Left("an error")
    val right: Either[String, Double] = Right(2)
    val f: (Int, Double) => String = (i, d) => (i * d).toString
    left.map2(right)(f) shouldBe Left("an error")

    right.map2(left)((d, i) => (i * d).toString) shouldBe Left("an error")
  }

  it should "return the first left if both Either-s are left" in {
    val left1: Either[String, Int] = Left("first error")
    val left2: Either[String, Int] = Left("second error")
    left1.map2(left2)(_ * _) shouldBe Left("first error")
  }

  it should "return the resulting Right if both Either-s are Right" in {
    val right1: Either[String, Int] = Right(2)
    val right2: Either[String, Double] = Right(3.0)
    right1.map2(right2)((i, d) => (i * d).toString) shouldBe Right("6.0")
  }

  it should "work with Try" in {
    val right: Either[Exception, Int] = Chapter04.TryE(2 / 1)
    val result = right.map2(Chapter04.TryE(1 / 0))(_ * _)
    result match {
      case Left(e) => e shouldBe a[ArithmeticException]
      case _ => fail
    }
  }
}