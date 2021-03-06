package chapter06

import org.scalatest.{FlatSpec, Matchers}

class RNGTest extends FlatSpec with Matchers {
  "Generating next integer" should "always return the same numbers with the same seed" in {
    val rng = SimpleRNG(123L)
    rng.nextInt()._1 shouldBe rng.nextInt()._1
  }

  it should "return a different number with the next RNG" in {
    val rng = SimpleRNG(123L)
    val (n, nextRng) = rng.nextInt()
    n shouldNot be(nextRng.nextInt()._1)
  }

  "Generating the next positive Integer" should "turn negative integers into their positive counterpart" in {
    val rng = SimpleRNG(-1L)
    val (n, _) = rng.nextInt()
    n < 0 shouldBe true

    val (nnn, _) = RNG.nonNegativeInt(rng)
    nnn shouldBe -n
  }

  "Generating a list of integers" should "return n integers" in {
    val rng = SimpleRNG(123L)
    val (ints, _) = RNG.ints(rng, 3)
    ints match {
      case _ :: _ :: _ :: Nil => ()
      case _ => fail
    }
  }


  "Generating a list of integers using sequence" should "return n integers" in {
    val rng = SimpleRNG(123L)
    val (ints, _) = RNG.intsUsingSequence(3)(rng)
    ints match {
      case _ :: _ :: _ :: Nil => ()
      case _ => fail
    }
  }

  "Generating bounded ints" should "work using state action rng" in {
    def testRange(start: Int, stop: Int) = {
      val rng = SimpleRNG(123L)
      val (ints, _) = RNG_USING_STATE_ACTION.intsBetween(start, stop, 10000).run(rng)
      (start until stop).foreach(ints.contains(_) shouldBe true)

      ints.find(_ < start) shouldBe None
      ints.find(_ >= stop) shouldBe None
    }

    testRange(-8, -3)
    testRange(-3, 5)
    testRange(3, 8)

    testRange(-1, 0)
    testRange(0, 1)
    testRange(-1, 1)
  }
}
