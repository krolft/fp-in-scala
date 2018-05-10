package chapter04

import scala.annotation.tailrec

/*
  4.1
 */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  // for 'default: => B' see test(s)
  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option[B]): Option[B]

  def filter(p: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this

  override def filter(p: A => Boolean): Option[A] = if (p(get)) this else None
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(p: Nothing => Boolean): Option[Nothing] = None
}

object Option {
  // 4.4
  def sequenceRec[A](as: List[Option[A]]): Option[List[A]] = {
    as match {
      case None :: _ => None
      case Some(a) :: tail => sequenceTailRec(tail).map(t => a :: t)
      case Nil => Some(Nil)
    }
  }

  // 4.4
  def sequenceTailRec[A](as: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(list: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = {
      list match {
        case None :: _ => None
        case Some(ha) :: tail => go(tail, acc.map(accList => ha :: accList))
        case Nil => acc.map(_.reverse)
      }
    }

    go(as, Some(Nil))
  }

  // 4.5
  def sequenceUsingTraverse[A](as: List[Option[A]]): Option[List[A]] = {
    traverse(as)(identity)
  }

  // 4.5
  def traverseUnefficient[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    sequenceTailRec(a.map(f))
  }

  // 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def go(list: List[A], acc: Option[List[B]]): Option[List[B]] = {
      list match {
        case None :: _ => None
        case h :: t => go(t, Chapter04.map2(acc, f(h))((accList, hb) => hb :: accList))
        case Nil => acc.map(_.reverse)
      }
    }

    go(as, Some(Nil))
  }

}
