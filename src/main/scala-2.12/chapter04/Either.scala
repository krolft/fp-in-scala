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