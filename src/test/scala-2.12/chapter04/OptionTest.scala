package chapter04

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {
  "Mapping an option" should "return None for a None" in {
    val none : Option[Int] = None
    none.map(_ + 1) shouldBe None
  }

  it should "return a Some with the mapped value for a Some" in {
    Some(1).map(_ + 1) shouldBe Some(2)
  }

  it should "not flatten options" in  {
    Some(1).map(a => Some(a + 1)) shouldBe Some(Some(2))
  }

  "Flat mapping an option" should "return None for a None" in {
    val none : Option[Int] = None
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
}
