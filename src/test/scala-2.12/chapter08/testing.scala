package chapter08

import org.scalatest.{FlatSpec, Matchers}

class testing extends FlatSpec with Matchers {

  private val trueProp = new Prop {
    override def check: Boolean = true
  }

  private val falseProp = new Prop {
    override def check: Boolean = false
  }

  "Chaining Props" should "for all combinations of two true and false Props" in {
    (trueProp && trueProp).check shouldBe true
    (trueProp && falseProp).check shouldBe false
    (falseProp && trueProp).check shouldBe false
    (falseProp && falseProp).check shouldBe false
  }
}
