package chapter03

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def tail[A](as: List[A]): List[A] =
    as match {
      // options would be
      // a) throwing an exception
      // b) additionally adding a method tailOption
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def drop[A](list: List[A], n: Int): List[A] = {
    @tailrec
    def go(i: Int, as: List[A]): List[A] = {
      as match {
        case Cons(x, xs) if i < n => go(i + 1, xs)
        case _ => as
      }
    }

    go(0, list)
  }

  /*
    it would also be possible to write the signature like this:
      def dropWhile[A](list: List[A], p: A => Boolean)
    in that case we need to help the compiler when calling it
      dropWhile(List(1, 2, 3), (x: Int) => x < 3) or
      dropWhile[Int](List(1, 2, 3), _ < 3)
    while with currying we don't need to
      dropWhile(List(1, 2, 3))(x => x < 3) or
      dropWhile(List(1, 2, 3))(_ < 3)
   */
  @tailrec
  def dropWhile[A](list: List[A])(p: A => Boolean): List[A] = {
    list match {
      case Cons(x, xs) if p(x) => dropWhile(xs)(p)
      case _ => list
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }

  def removeLast[A](list: List[A]): List[A] = {
    @tailrec
    def go(as: List[A], newList: List[A]): List[A] = {
      as match {
        case Cons(h, t: Cons[A]) => go(t, append(newList, List(h)))
        case _ => newList
      }
    }

    go(list, Nil)
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    list match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }

  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(xs: List[A], acc: B): B = {
      xs match {
        case Nil => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    }

    go(list, z)
  }

  /* 3.7
    will using foldRight enable taking care of 0.0 as element and short-circuiting?
    - with standard foldRight, it won't work
    - because foldRight doesn't know a collapsing zero element like 0.0 for product

    how could it work?
    - add a short-circuiting predicate and or collapsing element
    - scan for 0.0 before folding
   */
  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def setHead[A](as: List[A], head: A): List[A] =
    as match {
      case Nil => Cons(head, Nil)
      case Cons(_, xs) => Cons(head, xs)
    }

  def length[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  def sum(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def reverseTailRecursiveByHand[A](list: List[A]): List[A] = {
    @tailrec
    def go(as: List[A], newList: List[A]): List[A] = {
      as match {
        case Nil => newList
        case Cons(h, t) => go(t, Cons(h, newList))
      }
    }

    go(list, Nil)
  }

  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, List[A]())((newList, h) => Cons(h, newList))
  }

  def foldLeftNoTailRec[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
    list match {
      case Nil => z
      case Cons(h, t) => f(foldLeftNoTailRec(t, z)(f), h)
    }
  }

  def foldRightTailRec[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    @tailrec
    def go(xs: List[A], acc: B): B = {
      xs match {
        case Nil => acc
        case Cons(h, t) => go(t, f(h, acc))
      }
    }

    go(list, z)
  }

  /*
    FOLD RIGHT
    ```
    list match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
    ```
    foldRight(List(1, 2, 4), 0)(_ + _)
    1> 1 + foldRight(List(2, 4), 0)
    2> 1 + 2 + foldRight(List(4), 0)
    3> 1 + 2 + 4 + foldRight(Nil, 0)
    4> 1 + 2 + 4 + 0

    FOLD LEFT
    ```
    list match {
      case Nil => z
      case Cons(h, t) => f(foldLeft(t, z)(f), h)
    }
    ```
    foldLeft(List(1, 2, 4), 0)(_ + _)
    1> foldLeft(List(2, 4), 0) + 1
    2> foldLeft(List(2), 0) + 2 + 1
    3> foldLeft(Nil, 0) + 4 + 2 + 1
    4> 0 + 4 + 2 + 1

    FOLD LEFT with tail recursion
    ```
     def go(xs: List[A], acc: B): B = {
      xs match {
        case Nil => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    }
    go(list, z)
    ```
    foldLeft(List(1, 2, 4), 0)(_ + _)
    1> go(List(2, 4), 0 + 1)
    2> go(List(4), 0 + 1 + 2)
    3> go(Nil, 0 + 1 + 2 + 4)
    4> 0 + 1 + 2 + 4

    FOLD RIGHT using fold left with tail recursion
    ```
    foldLeft(list, z)((b, a) => f(a, b))
    ```
    foldRightUsingFoldLeft(List(1, 2, 4), 0)(_ + _)
    1> go(List(2, 4), 1 + 0)
    2> go(List(4), 2 + 1 + 0)
    3> go(Nil, 4 + 2 + 1 + 0)
    4> 4 + 2 + 1 + 0
   */
  // 3.13
  def foldRightUsingFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(list, z)((b, a) => f(a, b))
  }

  def foldLeftUsingFoldRight[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
    foldRightTailRec(list, z)((a, b) => f(b, a))
  }

  def appendUsingFold[A](xs: List[A], ys: List[A]): List[A] = {
    foldLeft(reverse(xs), ys)((acc, x) => Cons(x, acc))
    //foldLeftNoTailRec(xs, ys)((acc, x) => Cons(x, acc))
    //foldRight(xs, ys)((x, acc) => Cons(x, acc))
    //foldRightTailRec(reverse(xs), ys)((x, acc) => Cons(x, acc))
    //foldLeftUsingFoldRight(reverse(xs), ys)((acc, x) => Cons(x, acc))
    //foldRightUsingFoldLeft(reverse(xs), ys)((x, acc) => Cons(x, acc))
  }

  def concat[A](lists: List[List[A]]): List[A] = {
    foldLeft(lists, List[A]())(appendUsingFold)
  }

  // 3.16
  def add1(list: List[Int]): List[Int] = {
    def go(xs: List[Int], newList: List[Int]): List[Int] = {
      xs match {
        case Nil => reverse(newList)
        case Cons(h, t) => go(t, Cons(h + 1, newList))
      }
    }

    go(list, List())
  }

  // 3.17
  def doublesToStrings(list: List[Double]): List[String] = {
    def go(xs: List[Double], newList: List[String]): List[String] = {
      xs match {
        case Nil => reverse(newList)
        case Cons(h, t) => go(t, Cons(h.toString, newList))
      }
    }

    go(list, List())
  }

  // 3.18
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    def go(xs: List[A], newList: List[B]): List[B] = {
      xs match {
        case Nil => reverse(newList)
        case Cons(h, t) => go(t, Cons(f(h), newList))
      }
    }

    go(list, List())
  }

  // 3.19
  def filter[A](list: List[A])(p: A => Boolean): List[A] = {
    def go(xs: List[A], newList: List[A]): List[A] = {
      xs match {
        case Nil => reverse(newList)
        case Cons(h, t) if p(h) => go(t, Cons(h, newList))
        case Cons(_, t) => go(t, newList)
      }
    }

    go(list, List())
  }

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    def go(xs: List[A], newList: List[B]): List[B] = {
      xs match {
        case Nil => newList
        case Cons(h, t) => go(t, appendUsingFold(newList, f(h)))
      }
    }

    go(as, List())
  }

  // 3.21
  def filterWithFlatMap[A](list: List[A])(p: A => Boolean): List[A] = {
    flatMap(list)(a => if (p(a)) List(a) else List())
  }

  // 3.22
  def zipByAddingInts(list1: List[Int], list2: List[Int]): List[Int] = {
    def go(xs: List[Int], ys: List[Int], newList: List[Int]): List[Int] = {
      (xs, ys) match {
        case (Cons(xh, xt), Cons(yh, yt)) => go(xt, yt, Cons(xh + yh, newList))
        case _ => reverse(newList)
      }
    }

    go(list1, list2, List())
  }

  // 3.23
  def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = {
    def go(xs: List[A], ys: List[B], newList: List[C]): List[C] = {
      (xs, ys) match {
        case (Cons(xh, xt), Cons(yh, yt)) => go(xt, yt, Cons(f(xh, yh), newList))
        case _ => reverse(newList)
      }
    }

    go(list1, list2, List())
  }

  // 3.24
  def hasSubsequence[A](xs: List[A], sub: List[A]): Boolean = {
    xs match {
      case Nil => false
      case _ if List.length(xs) < List.length(sub) => false
      case Cons(_, t) =>
        zipWith(xs, sub)((x, s) => if (x == s) 0 else 1) match {
          case res if List.sum(res) == 0 => true
          case _ => hasSubsequence(t, sub)
        }
    }
  }

  // apply() implementation from the book
  // replaced it with own tail recursive implementation
  // ': _*' is a special type parameter allowing sequences to be passed as variadic parameters
  def applyNotTailRecursive[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, applyNotTailRecursive(as.tail: _*))

  def apply[A](list: A*): List[A] = {
    def go(as: Seq[A], newList: List[A]): List[A] = {
      if (as.isEmpty) newList
      else go(as.tail, Cons(as.head, newList))
    }

    // since creating a new list from a sequence will reverse the order,
    // we reverse the sequence first
    go(list.reverse, Nil)
  }

}