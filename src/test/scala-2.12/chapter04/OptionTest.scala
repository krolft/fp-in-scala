package chapter04

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {
  "Mapping an option" should "return None for a None" in {
    val none: Option[Int] = None
    none.map(_ + 1) shouldBe None
  }

  it should "return a Some with the mapped value for a Some" in {
    Some(1).map(_ + 1) shouldBe Some(2)
  }

  it should "not flatten options" in {
    Some(1).map(a => Some(a + 1)) shouldBe Some(Some(2))
  }

  "Flat mapping an option" should "return None for a None" in {
    val none: Option[Int] = None
    none.flatMap(a => Some(a + 1)) shouldBe None
  }

  it should "return a Some with the mapped value for a Some" in {
    Some(1).flatMap(a => Some(a + 1)) shouldBe Some(2)
  }

  "Get-or-else" should "return default value for a None" in {
    None.getOrElse(3.3) shouldBe 3.3
  }

  it should "return Some's value for a Some" in {
    Some(1).getOrElse(test()) shouldBe 1.0
  }

  // def getOrElse[B >: A](default: => B): B
  // with 'default: => B' test() does not get called for Some, only for None
  // with 'default: B' test() would be called for Some and None
  //  so it is some kind of lazy evaluation ...
  def test(): Double = {
    println("lala")
    3.3
  }

  "Or-else" should "return default option for a None" in {
    None.orElse(Some(3.3)) shouldBe Some(3.3)
    None.orElse(None) shouldBe None
  }

  it should "return Some's value for a Some" in {
    Some(1).orElse(Some(3.3)) shouldBe Some(1.0)
    Some(1).orElse(Some(3.3)) shouldBe Some(1)
    Some(1).orElse(Some(3)) shouldBe Some(1.0)
    // ---> if orElse is used the type of value can be any super type
    //      it doesn't matter if the parameter is of a super type or not

    Some(1).orElse(None) shouldBe Some(1)
  }

  "Filtering an option" should "return None for a None even when predicate always returns true" in {
    None.filter(_ => true) shouldBe None
  }

  it should "return a Some object with the same value for a predicate based on the value evaluating to true" in {
    Some(1).filter(_ % 2 == 1) shouldBe Some(1)
  }

  it should "return none for a predicate based on the value evaluating to false" in {
    Some(1).filter(_ % 2 == 0) shouldBe None
  }

  "Sequence a list of options into an option of list" should "return a Some of an empty list" in {
    Option.sequenceTailRec(List[Option[Int]]()) shouldBe Some(List())
    Option.sequenceRec(List[Option[Int]]()) shouldBe Some(List())
    Option.sequenceUsingTraverse(List[Option[Int]]()) shouldBe Some(List())
  }

  it should "return None for a list containing only a None" in {
    Option.sequenceTailRec(List[Option[Int]](None)) shouldBe None
    Option.sequenceRec(List[Option[Int]](None)) shouldBe None
    Option.sequenceUsingTraverse(List[Option[Int]](None)) shouldBe None
  }

  it should "return None for a list containing a None independent from where the None is located" in {
    Option.sequenceTailRec(List(None, Some(1), Some(2))) shouldBe None
    Option.sequenceTailRec(List(Some(1), None, Some(2))) shouldBe None
    Option.sequenceTailRec(List(Some(1), Some(2), None)) shouldBe None

    Option.sequenceRec(List(None, Some(1), Some(2))) shouldBe None
    Option.sequenceRec(List(Some(1), None, Some(2))) shouldBe None
    Option.sequenceRec(List(Some(1), Some(2), None)) shouldBe None

    Option.sequenceRec(List(None, Some(1), Some(2))) shouldBe None
    Option.sequenceRec(List(Some(1), None, Some(2))) shouldBe None
    Option.sequenceUsingTraverse(List(Some(1), Some(2), None)) shouldBe None
  }

  it should "return a Some containing the list of values in the some order if there is no None" in {
    Option.sequenceTailRec(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    Option.sequenceRec(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
    Option.sequenceUsingTraverse(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
  }

  "Traversing a list of values with a function producing an Option" should "return a Some of an empty list" in {
    Option.traverse(List[String]())(str => Chapter04.Try(str.toInt)) shouldBe Some(List())
  }

  it should "return None for a list containing only a None" in {
    Option.traverse(List[String]("abc"))(str => Chapter04.Try(str.toInt)) shouldBe None
  }

  it should "return None for a list containing a None independent from where the None is located" in {
    Option.traverse(List("abc", "1", "2"))(str => Chapter04.Try(str.toInt)) shouldBe None
    Option.traverse(List("1", "abc", "2"))(str => Chapter04.Try(str.toInt)) shouldBe None
    Option.traverse(List("1", "2", "abc"))(str => Chapter04.Try(str.toInt)) shouldBe None
  }

  it should "return a Some containing the list of values in the some order if there is no None" in {
    Option.traverse(List("1", "2", "3"))(str => Chapter04.Try(str.toInt)) shouldBe Some(List(1, 2, 3))
  }
}
