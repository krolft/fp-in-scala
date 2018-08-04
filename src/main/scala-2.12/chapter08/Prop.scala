package chapter08

import java.util.concurrent.atomic.AtomicInteger

import chapter05.Stream
import chapter06.{RNG, SimpleRNG}
import chapter08.Prop._

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  override def isFalsified: Boolean = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}
case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def tag(str: String): Prop = Prop { (m, n, rng) =>
    run(m, n, rng) match {
      case f: Falsified => f.copy(failure = s"$str\n${f.failure}")
      case ok@(Passed | Proved) => ok
    }
  }

  def &&(p: Prop): Prop = {
    Prop { (m, n, rng) =>
      run(m, n, rng) match {
        case f: Falsified => f
        case ok@(Passed | Proved) => ok -> p.run(m, n, rng) match {
          case (Proved, Proved) => Proved
          case (_, f: Falsified) => f
          case _ => Passed
        }
      }
    }
  }

  def ||(p: Prop): Prop = {
    Prop { (m, n, rng) =>
      run(m, n, rng) match {
        case ok@(Passed | Proved) => ok
        case f: Falsified => p.tag(f.failure).run(m, n, rng)
      }
    }
  }
}

object Prop {

  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int

  implicit val cnt: Map[String, AtomicInteger] = Stream.noEvalCounter

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())): Result =
    p.run(maxSize, testCases, rng)
}
