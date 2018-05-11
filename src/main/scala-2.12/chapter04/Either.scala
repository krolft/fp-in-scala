package chapter04

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, Nothing] = this

  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[E, Nothing] = this

  override def orElse[EE >: E, B](b: => Either[EE, B]): Either[EE, B] = b

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

  override def flatMap[E, B](f: A => Either[E, B]): Either[E, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    b.map(bb => f(value, bb))
}

object Either {
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] = {
    def go(list: List[Either[E, A]], acc: Either[E, List[A]]): Either[E, List[A]] = {
      list match {
        case Left(e) :: _ => Left(e)
        case Right(v) :: t => go(t, acc.map(l => v :: l))
        case Nil => acc.map(_.reverse)
      }
    }

    go(as, Right(List()))
  }

  def sequenceUsingTraverse[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def go(list: List[A], acc: Either[E, List[B]]): Either[E, List[B]] = {
      list match {
        case ha :: t => f(ha) match {
          case Right(vb) => go(t, acc.map(l => vb :: l))
          case Left(e) => Left(e)
        }
        case Nil => acc.map(_.reverse)
      }
    }

    go(as, Right(List()))
  }
}
