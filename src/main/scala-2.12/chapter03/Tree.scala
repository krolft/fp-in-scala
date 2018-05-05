package chapter03

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  def size[A](tree: Tree[A]): Long = {
    tree match {
      case _: Leaf[A] => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  // 3.29
  def sizeUsingFold[A](tree: Tree[A]): Long = {
    fold(tree)(_ => 1)(1 + _ + _)
  }

  // TODO?
  def sizeTailRec[A](tree: Tree[A]): Long = {
    ???
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  // 3.29
  def maximumUsingFold(tree: Tree[Int]): Int = {
    fold(tree)(a => a)(_ max _)
  }


  // 3.27
  def depth[A](tree: Tree[A]): Long = {
    tree match {
      case _: Leaf[A] => 1
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }

  // 3.29
  def depthUsingFold[A](tree: Tree[A]): Long = {
    fold(tree)(_ => 1)((b1, b2) => 1 + (b1 max b2))
  }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  // 3.29
  def mapUsingFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](tree)(v => Leaf(f(v)))((b1, b2) => Branch(b1, b2))
  }

  // 3.29
  def fold[A, B](tree: Tree[A])(fLeaf: A => B)(fBranch: (B, B) => B): B = {
    tree match {
      case Leaf(value) => fLeaf(value)
      case Branch(left, right) => fBranch(fold(left)(fLeaf)(fBranch), fold(right)(fLeaf)(fBranch))
    }
  }
}
