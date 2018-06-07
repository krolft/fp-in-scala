package chapter06


trait RNG {
  def nextInt(): (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt(): (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt()

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def mapCustom[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, nextRng) = s(rng)
      f(a) -> nextRng
    }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2Custom[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      f(a, b) -> rng3
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    //flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nextRng) = s(rng)
      val rand = f(a)
      rand(nextRng)
    }

  def sequenceTailRec[A](fs: List[Rand[A]]): Rand[List[A]] = startRng => {
    def go(xs: List[Rand[A]], rng: RNG, acc: List[A]): (List[A], RNG) = xs match {
      case Nil => (acc.reverse, rng)
      case h :: tail =>
        val (a, nextRng) = h(rng)
        go(tail, nextRng, a :: acc)
    }

    go(fs, startRng, Nil)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = startRng => {
    fs.foldRight(List[A]() -> startRng) {
      case (rand, (list, rng)) =>
        val (a, nextRng) = rand(rng)
        (a :: list) -> nextRng
    }
  }

  def nonNegativeInt: Rand[Int] = rng => {
    rng.nextInt() match {
      case (n, nextRng) if n == Int.MinValue => (0, nextRng)
      case (n, nextRng) if n < 0 => (-n, nextRng)
      case res => res
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))


  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def doubleCustom(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, nextRng)
  }

  def double: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)


  def intsCustom(startRng: RNG, count: Int): (List[Int], RNG) = {
    // TODO is there a better way? Like Stream.unfold?
    (0 until count).foldLeft[(List[Int], RNG)](List() -> startRng) {
      case ((acc, rng), _) =>
        val (n, nextRng) = rng.nextInt()
        (n :: acc, nextRng)
    }
  }

  def ints(startRng: RNG, count: Int): (List[Int], RNG) = {
    def go(i: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (i <= 0)
        (acc, rng)
      else {
        val (n, nextRng) = rng.nextInt()
        go(i - 1, n :: acc, nextRng)
      }
    }

    go(count, Nil, startRng)
  }

  def intsUsingSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))
}
