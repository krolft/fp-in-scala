package chapter04

sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B]

  def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B]

  def orElse[EE >: E, B >: A](b: => Validation[EE, B]): Validation[EE, B]
}

case class Failure[+E](error: E) extends Validation[E, Nothing] {
  override def map[B](f: Nothing => B): Validation[E, Nothing] = this

  override def flatMap[EE >: E, B](f: Nothing => Validation[EE, B]): Validation[E, Nothing] = this

  override def orElse[EE >: E, B >: Nothing](b: => Validation[EE, B]): Validation[EE, B] = ???
}

case class Success[+A](value: A) extends Validation[Nothing, A] {
  override def map[B](f: A => B): Validation[Nothing, B] = Success(f(value))

  override def flatMap[E, B](f: A => Validation[E, B]): Validation[E, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Validation[EE, B]): Validation[EE, B] = this
}
