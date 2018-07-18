package chapter08

import chapter06.{RNG, RNG_USING_STATE_ACTION, StateAction}

case class Gen[A](sample: StateAction[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.map(f).flatMap(_.sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen[Int](RNG_USING_STATE_ACTION.intBetween(start, stopExclusive))

  def choosePair(start: Int, stop: Int): Gen[(Int, Int)] =
    Gen(listOfN(2, choose(start, stop)).sample.map(list => (list.head, list.tail.head)))

  def unit[A](a: => A): Gen[A] =
    Gen[A](RNG_USING_STATE_ACTION.unit(a))

  def boolean: Gen[Boolean] =
    Gen[Boolean](RNG_USING_STATE_ACTION.boolean)

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen(StateAction.sequence(List.fill(n)(gen.sample)))

  def toOption[A](gen: Gen[A]): Gen[Option[A]] =
    Gen(gen.sample.map(Some(_)))

  def string(length: Int): Gen[String] = {
    /*
      // code from Random.scala
      def safeChar() = {
        val surrogateStart: Int = 0xD800
        val res = nextInt(surrogateStart - 1) + 1
        res.toChar
      }
     */
    val surrogateStart: Int = 0xD800

    Gen[String](listOfN(length,
      Gen(choose(0, surrogateStart).sample.map(_ + 1).map(_.toChar)))
      .sample.map(_.mkString)
    )
  }

  def union[A](a: Gen[A], b: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) a else b)
}
