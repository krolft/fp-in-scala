package chapter04

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  // for 'default: => B' see test(s)
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
}

case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
}
