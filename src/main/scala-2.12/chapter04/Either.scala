package chapter04

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
}
