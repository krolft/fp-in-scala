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
}
