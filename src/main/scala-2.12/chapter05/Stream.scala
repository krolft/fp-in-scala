package chapter05

import java.util.concurrent.atomic.AtomicInteger

sealed trait Stream[+A] {
  def headOptionCustom: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  def length: Long = this match {
    case Empty => 0
    case Cons(_, t) => 1 + t().length
  }

  def take(n: Long): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Long): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) if n <= 0 => Cons(h, () => t().drop(n))
    case _ => Empty
  }

  def takeWhileCustom(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhileCustom(p))
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
    foldRight(Stream.empty[A])((a, acc) => if (p(a)) Stream.cons(a, acc) else acc)

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
    foldRight[Stream[B]](Empty)((a, bs) => Stream.cons(f(a), bs))


  /*
    Input: Cons(() => 1, () => Cons(() => 2, () => Cons(() => 3, Cons(() => 4, () => Empty)))

    foldRight body:
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z

    f is defined as follows
      f = (a, acc) => if (p(a)) Stream.cons(a, acc) else acc)
      p = a % 2 == 0

    Step 1: this = Cons(() => 1, () => tail)
      f(1!, Cons(() => 2, () => tail).foldRight(Empty)(f))
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
}
