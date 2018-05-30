package chapter05

import java.util.concurrent.atomic.AtomicInteger
import Stream._

sealed trait Stream[+A] {
  def headOptionCustom: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Long)(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Long)(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) if n <= 0 => cons(h(), t().drop(n))
    case _ => Empty
  }

  def takeWhileCustom(p: A => Boolean)(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhileCustom(p))
    case _ => Empty
  }

  def existsCustom(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().existsCustom(p)
    case _ => false
  }

  // b within f(a, b) can be thought of as the not yet recursively evaluated tail
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /*
    Input: Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Empty)))

    foldRight body:
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z

    f and p are defined as follows
      f = (a, b) => p(a) && b)
      p = a < 2

    Step 1: this = Cons(() => 1, () => tail)

      f(1!, Cons(() => 2, () => tail').foldRight(true)(f))
      p(1!) && Cons(() => 2, () => tail').foldRight(true)(f)
      1! < 2 && Cons(() => 2, () => tail').foldRight(true)(f)
      true && Cons(() => 2, () => tail').foldRight(true)(f)
      // true && <right> // >>> right gets evaluated, which means tail gets traversed 1 time

    Step 2: this = Cons(() => 2, () => tail)

      true && f(2!, Cons(() => 3, () => tail').foldRight(true)(f)
      true && p(2!) && Cons(() => 3, () => tail').foldRight(true)(f)
      true && 2! < 2 && Cons(() => 3, () => tail').foldRight(true)(f)
      true && false && Cons(() => 3, () => tail').foldRight(true)(f)
      false && Cons(() => 3, () => tail').foldRight(true)(f)
      // false && <right> // >>> right is not evaluated
      false

    head needs to be evaluated 2 times
    tail needs to be traversed 1 time
   */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean)(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  /*
    Input: Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, () => Empty)))

    foldRight body:
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z

    f is defined as follows
      f = (a, _) => Some(a)

    Step 1: this = Cons(() => 1, () => tail)

      f(1!, Cons(() => 2, () => tail').foldRight(None)(f))
      Some(1!) // <right> is not used which means tail is not traversed

    head needs to be evaluated 1 time
    tail is never traversed because it is not used by f
   */
  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B)(implicit evalCounter: Map[String, AtomicInteger]): Stream[B] =
    foldRight[Stream[B]](Empty)((a, bs) => cons(f(a), bs))

  /*
    Input: Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, Cons(() => 4, () => Empty)))

    foldRight body:
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z

    f is defined as follows
      f = (a, acc) => if (p(a)) Stream.cons(a, acc) else acc)
      p = a % 2 == 0

    Step 1: this = Cons(() => 1, () => tail)
      f(1!, Cons(() => 2, () => tail').foldRight(Empty)(f))
      if (p(1!)) Stream.cons(a, Cons(() => 2, () => tail').foldRight(Empty)(f))
        else Cons(() => 2, () => tail').foldRight(Empty)(f))
      if (1! % 2 == 0)) Stream.cons(a, Cons(() => 2, () => tail').foldRight(Empty)(f))
        else Cons(() => 2, () => tail').foldRight(Empty)(f)
      Cons(() => 2, () => tail).foldRight(Empty)(f)

    Step 2: this = Cons(() => 2, () => tail)
      f(2!, Cons(() => 3, () => tail').foldRight(Empty)(f))
      if (p(2!)) Stream.cons(a, Cons(() => 3, () => tail').foldRight(Empty)(f))
        else Cons(() => 3, () => tail').foldRight(Empty)(f))
      if (2! % 2 == 0)) Stream.cons(2, Cons(() => 3, () => tail').foldRight(Empty)(f))
        else Cons(() => 3, () => tail').foldRight(Empty)(f)
      Stream.cons(2!, Cons(() => 3, () => tail').foldRight(Empty)(f))

    // head was evaluated two times (1 and 2)
    // tail was evaluated once
    // now the evaluation of the Stream stops
    // in general: the evaluation stops with the predicate evaluating to true the first time
   */
  def filter(p: A => Boolean)(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] =
    foldRight[Stream[A]](Empty)((a, acc) => if (p(a)) Stream.cons(a, acc) else acc)

  def append[AA >: A](xs: Stream[AA])(implicit evalCounter: Map[String, AtomicInteger]): Stream[AA] =
    foldRight[Stream[AA]](xs)((a, acc) => cons(a, acc))

  /*
      correct as by https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/laziness/Stream.scala
   */
  def flatMap[B](g: A => Stream[B])(implicit evalCounter: Map[String, AtomicInteger]): Stream[B] =
    foldRight[Stream[B]](Empty)((h, acc) => g(h) append acc)

  def find(p: A => Boolean)(implicit evalCounter: Map[String, AtomicInteger]): Option[A] =
    filter(p).headOption

  def mapUsingUnfold[B](f: A => B)(implicit evalCounter: Map[String, AtomicInteger]): Stream[B] =
    Stream.unfold[B, Stream[A]](this) {
      case Cons(h, t) => Some(f(h()) -> t())
      case Empty => None
    }

  def takeUsingUnfold(n: Int)(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] =
    Stream.unfold[A, (Int, Stream[A])]((n, this)) {
      case (i, Cons(h, t)) if i > 0 => Some(h() -> (i - 1 -> t()))
      case _ => None
    }

  def takeWhileUsingUnfold(p: A => Boolean)(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] =
    Stream.unfold[A, Stream[A]](this) {
      case Cons(h, t) if p(h()) => Some(h() -> t())
      case _ => None
    }

  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C)(implicit evalCounter: Map[String, AtomicInteger]): Stream[C] =
    Stream.unfold[C, (Stream[A], Stream[B])](this, stream) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()) -> (ta() -> tb()))
      case _ => None
    }

  def zip[B](stream: Stream[B])(implicit evalCounter: Map[String, AtomicInteger]): Stream[(A, B)] =
    Stream.unfold[(A, B), (Stream[A], Stream[B])](this, stream) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(ha() -> hb(), ta() -> tb())
      case _ => None
    }

  def zipAll[B](stream: Stream[B])(implicit evalCounter: Map[String, AtomicInteger]): Stream[(Option[A], Option[B])] =
    zipAllWith(stream)(_ -> _)

  def zipAllWith[B, C](stream: Stream[B])(f: (Option[A], Option[B]) => C)(implicit evalCounter: Map[String, AtomicInteger]): Stream[C] =
    Stream.unfold[C, (Stream[A], Stream[B])](this, stream) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(Some(ha()), Some(hb())), ta() -> tb())
      case (Cons(ha, ta), Empty) => Some((f(Some(ha()), None), ta() -> Empty))
      case (Empty, Cons(hb, tb)) => Some((f(None, Some(hb())), Empty -> tb()))
      case _ => None
    }

  def hasSubsequenceUsingZipAndForAll[AA >: A](stream: Stream[AA]): Boolean = this match {
    case Empty => false
    case Cons(_, t) => zipWith(stream)((x, s) => if (x == s) 0 else 1)(noEvalCounter) match {
      case res if res.forAll(_ == 0) => true
      case _ => t().hasSubsequence(stream)
    }
  }

  def startsWith[AA >: A](ys: Stream[AA]): Boolean = this match {
    case Empty => false
    case Cons(_, _) =>
      this.zipAll(ys)(noEvalCounter)
        .takeWhile {
          case (_, y) => y.isDefined
        }(noEvalCounter)
        .forAll {
          case (x, y) => x == y
        }
  }

  def tails(implicit evalCounter: Map[String, AtomicInteger]): Stream[Stream[A]] =
    unfold[Stream[A], Stream[A]](this) {
      case stream @ Cons(_, t) => Some(stream, t())
      case _ => None
    } append Stream(empty)

  def hasSubsequence[AA >: A](stream: Stream[AA]): Boolean = {
    implicit val nec: Map[String, AtomicInteger] = noEvalCounter
    tails.exists(_.startsWith(stream))
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A])(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] = {

    def count[B](key: String, value: B): B = {
      evalCounter.get(key).map(_.incrementAndGet)
      value
    }

    // head, tail => value calculation
    lazy val head = count("headVal", hd)
    lazy val tail = count("tailVal", tl)

    // head, tail => element traversal
    Cons(() => count("headTvs", head), () => count("tailTvs", tail))
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*)(implicit cnts: Map[String, AtomicInteger]): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def createEvalCounter =
    Map("headVal" -> new AtomicInteger(),
      "tailVal" -> new AtomicInteger(),
      "headTvs" -> new AtomicInteger(),
      "tailTvs" -> new AtomicInteger())

  private def noEvalCounter: Map[String, AtomicInteger] = Map()

  val ones: Stream[Int] = cons(1, ones)(noEvalCounter)

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))(noEvalCounter)

  def from(n: Int)(implicit cnts: Map[String, AtomicInteger]): Stream[Int] =
    cons(n, from(n + 1))(cnts)

  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a + b))(noEvalCounter)
    }

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)])(implicit cnts: Map[String, AtomicInteger]): Stream[A] =
    f(z) match {
      case Some((a, nextZ)) => cons(a, unfold(nextZ)(f))(cnts)
      case _ => empty
    }

  def fibsUsingUnfold(): Stream[Int] =
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }(noEvalCounter)

  def fromUsingUnfold(n: Int)(implicit cnts: Map[String, AtomicInteger]): Stream[Int] =
    unfold(n)(i => Some((i, i + 1)))

  def constantUsingUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))(noEvalCounter)

  val onesUsingUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))(noEvalCounter)
}
