package chapter08

import chapter06.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class GenTest extends FlatSpec with Matchers {

  "Generating a bounded int" should "work for minimal example" in {
    val sa = Gen.choose(1, 2).sample
    val generated = sa.run(SimpleRNG(123L)) match {
      case (v, _) => v
    }
    generated shouldBe 1
  }

  "Generating a list of bounded ints" should "work for an example" in {
    val (start, stop) = (1, 10)

    val gen = Gen.listOfN(10000, Gen.choose(start, stop))

    val ints = gen.sample.run(SimpleRNG(123L)) match {
      case (v, _) => v
    }

    (start until stop).foreach(ints.contains(_) shouldBe true)

    ints.find(_ < start) shouldBe None
    ints.find(_ >= stop) shouldBe None
  }

  it should "work for generated number of ints" in {
    val (start, stop) = (1, 10)

    val gen = Gen.choose(start, stop).listOfN(Gen.unit(10000))

    val ints = gen.sample.run(SimpleRNG(123L)) match {
      case (v, _) => v
    }

    (start until stop).foreach(ints.contains(_) shouldBe true)

    ints.find(_ < start) shouldBe None
    ints.find(_ >= stop) shouldBe None
  }

  "Generating a string" should "create a string with the specified length" in {
    val generated = Gen.string(1000).sample.run(SimpleRNG(123L)) match {
      case (str, _) => str
    }
    generated.length shouldBe 1000
  }
}
