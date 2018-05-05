package chapter03

object Chapter03 {
  def main(args: Array[String]) {
    println(List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(_, _) => 43
      case _ => 44
    })

    println(List.foldRight(List(1 to 10000: _*), 1)(_ + _))
  }
}
