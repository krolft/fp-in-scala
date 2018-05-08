package chapter03

import org.scalatest._

class TreeTest extends FlatSpec with Matchers {

  "Getting the size of a tree" should "return 1 for the smallest tree possible" in {
    Tree.size(Leaf(1)) shouldBe 1
    Tree.sizeUsingFold(Leaf(1)) shouldBe 1
  }

  it should "return the number of leafs and branches" in {
    Tree.size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 5
    Tree.sizeUsingFold(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) shouldBe 5
  }

  "Getting the maximum of a tree" should "work for the smallest tree possible" in {
    Tree.maximum(Leaf(123)) shouldBe 123
    Tree.maximumUsingFold(Leaf(123)) shouldBe 123
  }

  it should "return the maximum of all leafs" in {
    Tree.maximum(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) shouldBe 3
    Tree.maximumUsingFold(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) shouldBe 3
  }

  "Getting the depth of a tree" should "return 1 for the smallest tree possible" in {
    Tree.depth(Leaf(123)) shouldBe 1
    Tree.depthUsingFold(Leaf(123)) shouldBe 1
  }

  it should "return the depth of the longest branch" in {
    val tree =
      Branch(
        Leaf(1),
        Branch(
          Branch(
            Leaf(3),
            Branch(
              Leaf(5),
              Leaf(6)
            )
          ),
          Leaf(2)
        )
      )

    Tree.depth(tree) shouldBe 5
    Tree.depthUsingFold(tree) shouldBe 5
  }

  "Mapping a tree" should "work for the smallest tree possible" in {
    Tree.map(Leaf(123))(_ * 2 + "") shouldBe Leaf("246")
    Tree.mapUsingFold(Leaf(123))(_ * 2 + "") shouldBe Leaf("246")
  }

  it should "map all branches and leafs" in {
    Tree.map(Branch(Leaf(1), Branch(Leaf(3), Leaf(2))))(_ * 2 + "") shouldBe Branch(Leaf("2"), Branch(Leaf("6"), Leaf("4")))
    Tree.mapUsingFold(Branch(Leaf(1), Branch(Leaf(3), Leaf(2))))(_ * 2 + "") shouldBe Branch(Leaf("2"), Branch(Leaf("6"), Leaf("4")))
  }
}
