package chapter08

import org.scalatest.{FlatSpec, Matchers}

class PropTest extends FlatSpec with Matchers {

  "A Prop" should "be runnable" in {
    val smallInt = Gen.choose(-10, 10)
    val listOfSmallInts = SGen.listOf1(smallInt)
    val maxProp = Prop.forAll(listOfSmallInts)(list => {
      val max = list.max
      !list.exists(_ > max)
    })
    Prop.run(maxProp) shouldBe Passed
  }

  it should "work with &&" in {
    val smallInt = Gen.choose(-10, 10)
    val listOfSmallInts = SGen.listOf(smallInt)
    val propTrue = Prop.forAll(listOfSmallInts)(list => list.reverse.reverse == list).tag("True: reverse reversed")
    val propFalse = Prop.forAll(listOfSmallInts)(list => list.reverse == list).tag("False: reverse")

    Prop.run(propTrue && propFalse) should matchPattern {
      case f: Falsified if f.failure.startsWith("False") =>
    }
  }
}
