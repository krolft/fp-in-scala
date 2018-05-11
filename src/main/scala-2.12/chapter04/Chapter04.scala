package chapter04

object Chapter04 {
  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = xs match {
      case Nil => None
      case _ => Some(xs.sum / xs.length)
    }

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val absOpt: Option[Double] => Option[Double] = lift(Math.abs)

  // again: writing 'a: => A' instead of 'a: A' makes 'a' being evaluated (possibly raising an exception)
  // within our method, and not where Try is called
  // calling Try(1 / 0) will raise an exception within the Try method and not where 'Try(1 / 0)' is stated
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case _: Exception => None
    }
  }

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }
  }

  def TryE[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def safeDivUsingTry(x: Int, y: Int): Either[Exception, Int] =
    TryE(x / y)
}
