package chapter08

import chapter06.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class PropTest extends FlatSpec with Matchers {

  "A Prop" should "be runnable" in {
    val smallInt = Gen.choose(-10, 10)
    val listOfSmallInts = SGen.listOf(smallInt)
    val maxProp = Prop.forAll(listOfSmallInts) ( list =>
      if (list.isEmpty) true
      else {
        val max = list.max
        !list.exists(_ > max)
      }
    )
    maxProp.run(100, 100, SimpleRNG(123L)) shouldBe Passed
  }

  it should "work with &&" in {
    val smallInt = Gen.choose(-10, 10)
    val listOfSmallInts = SGen.listOf(smallInt)
    val propTrue = Prop.forAll(listOfSmallInts)(list => list.reverse.reverse == list).tag("True: reverse reversed")
    val propFalse = Prop.forAll(listOfSmallInts)(list => list.reverse == list).tag("False: reverse")

    (propTrue && propFalse).run(100, 100, SimpleRNG(123L)) should matchPattern {
      case f: Falsified if f.failure.startsWith("False") =>
    }
  }
}
