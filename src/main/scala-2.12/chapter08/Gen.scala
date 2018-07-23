package chapter08

import chapter06.{RNG, RNG_USING_STATE_ACTION, StateAction}

case class Gen[+A](sample: StateAction[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.map(f).flatMap(_.sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)
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

case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
}

object SGen {
  def choose(start: Int, stopExclusive: Int): SGen[Int] = Gen.choose(start, stopExclusive).unsized

  def choosePair(start: Int, stop: Int): SGen[(Int, Int)] = Gen.choosePair(start, stop).unsized

  def unit[A](a: => A): SGen[A] = Gen.unit(a).unsized

  def boolean: SGen[Boolean] = Gen.boolean.unsized

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n max 1, g))

  def string: SGen[String] = SGen(i => Gen.string(i))
}
