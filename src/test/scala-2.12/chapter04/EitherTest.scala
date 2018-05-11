package chapter04

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

  "Sequence a list of Either-s into an Either of list" should "return a Right for an empty list" in {
    Either.sequence(List[Either[String, Int]]()) shouldBe Right(List())
    Either.sequenceUsingTraverse(List[Either[String, Int]]()) shouldBe Right(List())
  }

  it should "return a Left for a list containing only a Left" in {
    Either.sequence(List[Either[String, Int]](Left("nope"))) shouldBe Left("nope")
    Either.sequenceUsingTraverse(List[Either[String, Int]](Left("nope"))) shouldBe Left("nope")
  }

  it should "return Left for a list containing a Left independent from where the Left is located" in {
    Either.sequence(List(Left("nope"), Right(1), Right(2))) shouldBe Left("nope")
    Either.sequence(List(Right(1), Left("nope"), Right(2))) shouldBe Left("nope")
    Either.sequence(List(Right(1), Right(2), Left("nope"))) shouldBe Left("nope")

    Either.sequenceUsingTraverse(List(Left("nope"), Right(1), Right(2))) shouldBe Left("nope")
    Either.sequenceUsingTraverse(List(Right(1), Left("nope"), Right(2))) shouldBe Left("nope")
    Either.sequenceUsingTraverse(List(Right(1), Right(2), Left("nope"))) shouldBe Left("nope")
  }

  it should "return the first Left for a list containing more than one Left" in {
    Either.sequence(List(Left("nope1"), Left("nope2"), Right(2))) shouldBe Left("nope1")
    Either.sequence(List(Right(1), Left("nope1"), Left("nope2"))) shouldBe Left("nope1")
    Either.sequence(List(Right(1), Left("nope1"), Left("nope2"))) shouldBe Left("nope1")

    Either.sequenceUsingTraverse(List(Left("nope1"), Left("nope2"), Right(2))) shouldBe Left("nope1")
    Either.sequenceUsingTraverse(List(Right(1), Left("nope1"), Left("nope2"))) shouldBe Left("nope1")
    Either.sequenceUsingTraverse(List(Right(1), Left("nope1"), Left("nope2"))) shouldBe Left("nope1")
  }

  it should "return a Some containing the list of values in the some order if there is no None" in {
    Either.sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
    Either.sequenceUsingTraverse(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
  }

  "Traversing a list of values with a function producing an Either" should "return a Right for an empty list" in {
    Either.traverse(List[String]())(str => Chapter04.TryE(str.toInt)) shouldBe Right(List())
  }

  it should "return Left for a list containing only a Left" in {
    Either.traverse(List[String]("abc"))(str => Chapter04.TryE(str.toInt)) shouldBe a[Left[_]]
  }

  it should "return Left for a list containing a Left independent from where the Left is located" in {
    Either.traverse(List("abc", "1", "2"))(str => Chapter04.TryE(str.toInt)) shouldBe a[Left[_]]
    Either.traverse(List("1", "abc", "2"))(str => Chapter04.TryE(str.toInt)) shouldBe a[Left[_]]
    Either.traverse(List("1", "2", "abc"))(str => Chapter04.TryE(str.toInt)) shouldBe a[Left[_]]
  }

  it should "return a Right containing the list of values in the same order if there is no Left" in {
    Either.traverse(List("1", "2", "3"))(str => Chapter04.TryE(str.toInt)) shouldBe Right(List(1, 2, 3))
  }
}