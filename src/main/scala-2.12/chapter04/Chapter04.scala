package chapter04

object Chapter04 {
  def variance(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None
    case _ =>
      val mean = xs.sum / xs.length
      Some(xs.map(x => math.pow(x - mean, 2)).sum / xs.length)
  }
}
