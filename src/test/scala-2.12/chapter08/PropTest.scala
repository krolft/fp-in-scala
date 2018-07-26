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

  it should "enable testing of List.sorted" in {
    val smallInt = Gen.choose(-10, 10)
    val listOfSmallInts = SGen.listOf(smallInt)

    val sortedCompare = Prop.forAll(listOfSmallInts)(list => {
      // my solution
      //list.sorted.foldLeft(Int.MinValue -> true) { case ((last, valid), cur) => cur -> (valid && last <= cur) }._2

      // usage of zip is inspired by book
      list.sorted match {
        case xs @ _ :: t => !xs.zip(t).exists { case (x, y) => x > y }
        case Nil => true
      }
    }).tag("comparision")

    val sortedTwice = Prop.forAll(listOfSmallInts)(list =>
      list.sorted.sorted == list.sorted
    ).tag("resorting")

    val sortedLength = Prop.forAll(listOfSmallInts)(list =>
      list.sorted.length == list.length
    ).tag("length")

    val sortedElements = Prop.forAll(listOfSmallInts)(list => {
      val sortedList = list.sorted
      sortedList.forall(list.contains) && list.forall(sortedList.contains)
    }).tag("elements")

    Prop.run(sortedCompare && sortedTwice && sortedLength && sortedElements) shouldBe Passed
  }
}
