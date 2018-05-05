import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(i: Int, acc: Int): Int = {
      if (i <= 1) acc
      else go(i - 1, acc * i)
    }

    go(n, 1)
  }

  def fibonacci(n: Int): Int = {
    @tailrec
    def go(i: Int, curElement: Int, nextElement: Int): Int = {
      if (i == n) curElement
      else go(i + 1, nextElement, curElement + nextElement)
    }

    go(0, 0, 1)
  }

  private def format(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def fibonacciStillTailRecursive(n: Int): Int = {
    @tailrec
    def go(i: Int, curElement: Int, nextElement: Int): Int = {
      if (i < n && i % 2 == 0)
        go(i + 1, nextElement, curElement + nextElement)
      else if (i < n && i % 2 == 1)
        go(i + 1, nextElement, curElement + nextElement)
      else curElement
    }

    go(0, 0, 1)
  }

  // stack overflow e.g. for addNotTailRecursive(1, 100000)
  def addNotTailRecursive(a: Int, b: Int): Int = {

    // @tailrec ->
    //  "error: could not optimize @tailrec annotated method go: it contains a recursive call not in tail position"
    //  because it does computation using the result of the call to go
    def go(i: Int, acc: Int): Int = {
      if (i >= b) acc
      else 0 + go(i + 1, acc + 1)
    }

    go(0, a)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else go(i + 1)
    }

    go(0)
  }

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    (b) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a) => (b) => f(a, b)

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: A => B, g: B => C): (A) => C =
    (a) => g(f(a))

  def main(args: Array[String]) {
    println(format("absolute", -7, abs))
    println(format("factorial", 7, factorial))
    println(format("fibonacci", 7, fibonacci))
  }
}
