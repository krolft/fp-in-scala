package chapter05

import java.util.concurrent.atomic.AtomicInteger

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
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
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A])(implicit evalCounter: Map[String, AtomicInteger]): Stream[A] = {

    def count(key: String): Unit = {
      evalCounter.get(key).map(_.incrementAndGet)
    }

    // cons => evaluate cons method tree, replace with Cons element tree
    count("cons")

    // head, tail => value calculation
    lazy val head = {count("headVal"); hd}
    lazy val tail = {count("tailVal"); tl}

    // head, tail => element traversal
    Cons(() => {
      count("headTvs"); head
    }, () => {
      count("tailTvs"); tail
    })
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*)(implicit cnts: Map[String, AtomicInteger]): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def createEvalCounter =
    Map("headVal" -> new AtomicInteger(),
      "tailVal" -> new AtomicInteger(),
      "headTvs" -> new AtomicInteger(),
      "tailTvs" -> new AtomicInteger(),
      "cons" -> new AtomicInteger())

}
