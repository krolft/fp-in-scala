package chapter08

import java.util.concurrent.atomic.AtomicInteger

import chapter05.Stream
import chapter06.RNG
import chapter08.Prop._

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  override def isFalsified: Boolean = false
}
case class Failed(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = {
    Prop { (n, rng) =>
      run(n, rng) match {
        case f: Failed => f
        case Passed => p.run(n, rng)
      }
    }
  }

  def ||(p: Prop): Prop = {
    Prop { (n, rng) =>
      run(n, rng) match {
        case Passed => Passed
        case _: Failed => p.run(n, rng)
      }
    }
  }
}

object Prop {

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  implicit val cnt: Map[String, AtomicInteger] = Stream.noEvalCounter

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed
          else Failed(a.toString, i)
        }
        catch {
          case e: Exception => Failed(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}
