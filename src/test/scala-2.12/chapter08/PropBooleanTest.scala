package chapter08

import org.scalatest.{FlatSpec, Matchers}

class PropBooleanTest extends FlatSpec with Matchers {

  private val trueProp = new PropBoolean {
    override def check: Boolean = true
  }

  private val falseProp = new PropBoolean {
    override def check: Boolean = false
  }

  "Chaining Props returning a Boolean" should "for all combinations of two true and false Props" in {
    (falseProp && falseProp).check shouldBe false
    (falseProp && trueProp).check shouldBe false
    (trueProp && falseProp).check shouldBe false
    (trueProp && trueProp).check shouldBe true
  }
}
