package chapter08

import chapter06.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class PropTest extends FlatSpec with Matchers {

  "A Prop" should "be runnable" in {
    val gen = Gen.choose(-5, 5).listOfN(Gen.choose(2, 10))
    val prop = Prop.forAll(gen)(list => list.reverse.reverse == list)
    prop.run(5, SimpleRNG(123L)) shouldBe Passed
  }

  it should "work with &&" in {
    val gen = Gen.choose(-5, 5).listOfN(Gen.choose(2, 10))
    val propTrue = Prop.forAll(gen)(list => list.reverse.reverse == list)
    val propFalse = Prop.forAll(gen)(list => list.reverse == list)

    (propTrue && propFalse).run(5, SimpleRNG(123L)) shouldNot be(Passed)
  }
}
