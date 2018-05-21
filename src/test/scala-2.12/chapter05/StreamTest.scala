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
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 0
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 0
    localEvalCount shouldBe 3

    stream.toList shouldBe List(1, 2, 3)

    mapEvalCount("headTvs") shouldBe 3
    mapEvalCount("tailTvs") shouldBe 3
    mapEvalCount("headVal") shouldBe 3
    mapEvalCount("tailVal") shouldBe 3
    localEvalCount shouldBe 3

    stream.toList shouldBe List(1, 2, 3)

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
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 0
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 0

    stream.toList shouldBe List(1, 2, 3)
    localEvalCount shouldBe 3
    mapEvalCount("headTvs") shouldBe 3
    mapEvalCount("tailTvs") shouldBe 3
    mapEvalCount("headVal") shouldBe 3
    mapEvalCount("tailVal") shouldBe 3
  }

  "Getting the length of a Stream" should "not require to evaluate values and should only traverse the tail" in {
    val stream = Stream.cons(countEval(1), Stream.cons(countEval(2), Stream.cons(countEval(3), Stream.empty)))

    localEvalCount shouldBe 0
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 0
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 0

    stream.length shouldBe 3
    localEvalCount shouldBe 0
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 3
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 3

    stream.length shouldBe 3
    localEvalCount shouldBe 0
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 6
    mapEvalCount("headVal") shouldBe 0
    mapEvalCount("tailVal") shouldBe 3
  }

  "Creating a new Stream from the first n elements of a Stream" should
    "not require to traverse or evaluate a Stream until materialization" in {
    val stream = Stream.cons(countEval(1), Stream.cons(countEval(2), Stream.cons(countEval(3), Stream.empty)))

    val stream12 = stream.take(2)

    localEvalCount shouldBe 0
    // head is never traversed / evaluated, only passed to the new Stream
    // tail is only passed non-strict to the new Stream
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 0

    // only through toList are heads and tails traversed
    stream12.toList shouldBe List(1, 2)
  }

  it should "only take as many elements as the Stream contains" in {
    Stream(1, 2, 3).take(6).toList shouldBe List(1, 2, 3)
  }

  "Dropping the first n elements of a stream" should "only traverse the n first tails" in {
    val stream = Stream.cons(countEval(1), Stream.cons(countEval(2), Stream.cons(countEval(3), Stream.empty)))

    val stream3 = stream.drop(2)

    localEvalCount shouldBe 0
    // head is never traversed / evaluated, only neglected or passed to the new Stream
    // tail is traversed n times and then passed non-strict to the new Stream
    mapEvalCount("headTvs") shouldBe 0
    mapEvalCount("tailTvs") shouldBe 2

    // only through toList are heads traversed
    stream3.toList shouldBe List(3)
  }

  it should "only drop as many as there are" in {
    Stream(1, 2, 3).drop(6).toList shouldBe List()
  }

  "Taking the first elements of a stream while a predicate is true" should "only evaluate the first head" in {
    val stream = Stream.cons(countEval(1), Stream.cons(countEval(2), Stream.cons(countEval(3), Stream.empty)))

    val stream12 = stream.takeWhile(_ < 3)

    // only the first head will be evaluated, then takeWhile is called on tail in a non strict fashion
    localEvalCount shouldBe 1
    mapEvalCount("headTvs") shouldBe 1
    mapEvalCount("headVal") shouldBe 1

    // tail is not traversed but only passed no strict to the new Stream
    mapEvalCount("tailTvs") shouldBe 0

    stream12.toList shouldBe List(1, 2)
  }

  "Checking if a predicate is true for an element in a stream" should
    "evaluate as many heads until if finds one being true" in {

    val stream = Stream.cons(countEval(1), Stream.cons(countEval(2), Stream.cons(countEval(3), Stream.empty)))

    stream.exists(_ == 2) shouldBe true

    // only the first head will be evaluated, then takeWhile is called on tail in a non strict fashion
    localEvalCount shouldBe 2
    mapEvalCount("headTvs") shouldBe 2

    // tail is not traversed but only passed no strict to the new Stream
    mapEvalCount("tailTvs") shouldBe 1
  }

  "Checking if a predicate holds true for all elements" should "stop as soon as one is false" in {
    Stream(1, 2, 3, 4).forAll(_ < 2) shouldBe false

    mapEvalCount("headTvs") shouldBe 2
    mapEvalCount("tailTvs") shouldBe 1
  }

  "Getting the first element of a Stream" should "only evaluate head and not touch tail" in {
    Stream(1, 2, 3).headOption shouldBe Some(1)
    mapEvalCount("headTvs") shouldBe 1
    mapEvalCount("headVal") shouldBe 1
    mapEvalCount("tailTvs") shouldBe 0
    mapEvalCount("tailVal") shouldBe 0

    Stream.empty.headOption shouldBe None
  }

  def countEval(i: Int): Int = {
    localEvalCount = localEvalCount + 1
    i
  }

  private def mapEvalCount(ofKey: String)(implicit evalCounter: Map[String, AtomicInteger]) = {
    evalCounter(ofKey).get
  }
}
