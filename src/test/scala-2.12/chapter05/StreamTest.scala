package chapter05

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  var localEvalCount: Int = _

  implicit var mapInternalEvalCounter: Map[String, AtomicInteger] = _

  override def beforeEach() {
    localEvalCount = 0
    mapInternalEvalCounter = Stream.createEvalCounter
  }

  "Turning a Stream into a List" should "return an empty List for an empty Stream" in {
    Stream.empty.toList shouldBe List()
  }

  it should "return a List with the elements in the same order as the Stream" in {
    val stream = Stream(countEval(1), countEval(2), countEval(3))

    // The first call to cons will be replaced by a Cons, the remaining cons do not.
    // Cons keep the counting mechanism, counting when head and tail are accessed.
    // When all cons have been evaluated to Cons, the Stream structure is built up in memory and cons counting stops
    // while the values may still be unevaluated (test with length)
    mapEvalCount("cons") shouldBe 1
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 0
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 0
    localEvalCount shouldBe 3

    stream.toList shouldBe List(1, 2, 3)

    mapEvalCount("cons") shouldBe 3
    mapEvalCount("headTvs") shouldBe 3
    mapEvalCount("tailTvs") shouldBe 3
    mapEvalCount("headVal") shouldBe 3
    mapEvalCount("tailVal") shouldBe 3
    localEvalCount shouldBe 3

    stream.toList shouldBe List(1, 2, 3)

    mapEvalCount("cons") shouldBe 3
    // traversing the structure will occur as often as elements times number calls to toList
    mapEvalCount("headTvs") shouldBe 6
    mapEvalCount("tailTvs") shouldBe 6
    // headVal and tailVal are evaluated lazily and will not increase above the number of elements
    mapEvalCount("headVal") shouldBe 3
    mapEvalCount("tailVal") shouldBe 3
    localEvalCount shouldBe 3
  }

  it should "not evaluate the values locally using Stream.cons instead of Streams.apply" in {
    val stream = Stream.cons(countEval(1), Stream.cons(countEval(2), Stream.cons(countEval(3), Stream.empty)))

    localEvalCount shouldBe 0
    mapEvalCount("cons") shouldBe 1
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 0
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 0

    stream.toList shouldBe List(1, 2, 3)
    localEvalCount shouldBe 3
    mapEvalCount("cons") shouldBe 3
    mapEvalCount("headTvs") shouldBe 3
    mapEvalCount("tailTvs") shouldBe 3
    mapEvalCount("headVal") shouldBe 3
    mapEvalCount("tailVal") shouldBe 3
  }

  "Getting the length of a Stream" should "not require to evaluate values and should only traverse the tail" in {
    val stream = Stream.cons(countEval(1), Stream.cons(countEval(2), Stream.cons(countEval(3), Stream.empty)))

    localEvalCount shouldBe 0
    mapEvalCount("cons") shouldBe 1
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 0
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 0

    stream.length shouldBe 3
    localEvalCount shouldBe 0
    mapEvalCount("cons") shouldBe 3
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 3
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 3

    stream.length shouldBe 3
    localEvalCount shouldBe 0
    mapEvalCount("cons") shouldBe 3
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 6
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 3
  }

  def countEval(i: Int): Int = {
    localEvalCount = localEvalCount + 1
    i
  }

  private def mapEvalCount(ofKey: String)(implicit evalCounter: Map[String, AtomicInteger]) = {
    evalCounter(ofKey).get
  }
}
