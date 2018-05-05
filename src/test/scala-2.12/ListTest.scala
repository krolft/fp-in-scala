import org.scalatest._

class ListTest extends FlatSpec with Matchers {

  "Retrieving the tail of a list" should "return an empty list for empty lists" in {
    List.tail(List()) shouldBe List()
  }

  it should "return the remaining elements" in {
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  "Setting the head on a list" should "return the element as list for empty lists" in {
    List.setHead(List(), 1) shouldBe List(1)
  }

  it should "replace the head element" in {
    List.setHead(List(1, 2, 3), 4) shouldBe List(4, 2, 3)
  }

  "Dropping n elements from list" should "return an empty list for an empty list" in {
    List.drop(List(), 3) shouldBe List()
  }

  it should "drop the first n elements" in {
    List.drop(List(1, 2, 3, 4), 2) shouldBe List(3, 4)
  }

  it should "drop nothing if n is zero" in {
    List.drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
  }

  it should "drop nothing if n is negative" in {
    List.drop(List(1, 2, 3), -1) shouldBe List(1, 2, 3)
  }

  it should "drop the only element" in {
    List.drop(List(1), 1) shouldBe List()
  }

  "Dropping while predicate" should "return an empty list for an empty list" in {
    List.dropWhile(List())(_ => true) shouldBe List()
  }

  it should "end even if predicate always returns true" in {
    List.dropWhile(List(1, 2, 3))(_ => true) shouldBe List()
  }

  it should "not change the list if predicate is not matching" in {
    List.dropWhile(List(1, 2, 3))(_ => false) shouldBe List(1, 2, 3)
  }

  it should "remove only from the beginning" in {
    List.dropWhile(List(1, 3, 5, 6, 7))(_ % 2 == 1) shouldBe List(6, 7)
  }

  "Removing the last element" should "return an empty list for an empty list" in {
    List.removeLast(List()) shouldBe List()
  }

  it should "return an empty list for a list with one element" in {
    List.removeLast(List(1)) shouldBe List()
  }

  it should "return a list with the last element missing" in {
    List.removeLast(List(1, 2, 3)) shouldBe List(1, 2)
  }

  "Tail recursive apply method" should "return empty list when called with empty parameter list" in {
    List.apply() shouldBe Nil
  }

  it should "contain the given parameters in the same order" in {
    List.apply(1, 2, 3) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
    List.applyNotTailRecursive(1, 2, 3) shouldBe Cons(1, Cons(2, Cons(3, Nil)))
  }

  "Folding the list" should "return start value with empty list" in {
    List.foldRight(List[Int](), -123)((x, acc) => x + acc) shouldBe -123
  }

  it should "work for adding ints" in {
    List.foldRight(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
  }

  /*
    3.8
    ... TODO see answers
   */
  it should "enable us to create a copy of a list" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
  }

  "Getting the length of a list" should "return 0 for empty list" in {
    List.length(List()) shouldBe 0
  }

  it should "return the length of an non empty list" in {
    List.length(List(1, 2, 3)) shouldBe 3
  }

  "Folding the list left with tail recursion" should "return start value for empty list" in {
    List.foldLeft(List[Int](), 123)(_ + _) shouldBe 123
  }

  it should "work for adding ints" in {
    List.foldLeft(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
  }

  it should "concatenate start value and elements" in {
    List.foldLeft(List(1, 2, 3), "0")(_ + _) shouldBe "0123"
  }

  "Summing up ints" should "work" in {
    List.sum(List(1, 2, 3)) shouldBe 6
  }

  "Multiplying up doubles" should "work" in {
    List.product(List(2, 2.5, 3)) shouldBe 15
  }

  "Reversing a list" should "return empty list for a given empty list" in {
    List.reverse(List()) shouldBe List()
  }

  it should "return the elements of the list in reversed order" in {
    List.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  "Folding the list left without tail recursion" should "return start value for empty list" in {
    List.foldLeftNoTailRec(List[Int](), 123)(_ + _) shouldBe 123
  }

  it should "work for adding ints" in {
    List.foldLeftNoTailRec(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
  }

  it should "concatenate start value and elements" in {
    List.foldLeftNoTailRec(List(1, 2, 3), "0")(_ + _) shouldBe "0321"
  }

  "Folding the list right with tail recurcsion" should "return start value for empty list" in {
    List.foldRightTailRec(List[Int](), 123)(_ + _) shouldBe 123
  }

  it should "work for adding ints" in {
    List.foldRightTailRec(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
  }

  "Append" should "append two lists to one containing all elements in the same order" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Append using fold" should "append two lists to one containing all elements in the same order" in {
    List.appendUsingFold(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Concatenating all elements of a list of lists" should "create a list with all elements in the right order" in {
    List.concat(List(List('a', 'b', 'c'), List('d'), List('e', 'f'))) shouldBe List('a', 'b', 'c', 'd', 'e', 'f')
  }

  "Add one to all elements" should "return empty list when called with empty list" in {
    List.add1(List()) shouldBe List()
  }

  it should "add one to all elements" in {
    List.add1(List(-1, 0, 1, 2)) shouldBe List(0, 1, 2, 3)
  }

  "Transform all double elements to strings" should "return empty list when called with empty list" in {
    List.doublesToStrings(List()) shouldBe List()
  }

  it should "transform all double elements to strings" in {
    List.doublesToStrings(List(-1.0, 0.0, 1.5, 2)) shouldBe List("-1.0", "0.0", "1.5", "2.0")
  }

  "Mapping all elements" should "return empty list when called with empty list" in {
    List.map(List[Int]())(_ + 1) shouldBe List()
  }

  it should "add one to all elements" in {
    List.map(List(-1, 0, 1, 2))(_ + 1) shouldBe List(0, 1, 2, 3)
  }

  it should "transform all double elements to strings" in {
    List.map(List(-1.0, 0.0, 1.5, 2))(_.toString) shouldBe List("-1.0", "0.0", "1.5", "2.0")
  }

  "Filtering odd numbers" should "return a List containing only the even numbers given" in {
    List.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
  }

  "Flat map" should "work for the given example" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "Filtering odd numbers using flat map" should "return a List containing only the even numbers given" in {
    List.filterWithFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
  }

  "Fold right using fold left" should "let us sum ints" in {
    List.foldRightUsingFoldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
  }

  it should "apply the function and elements in the same order as tail recursive fold left does" in {
    List.foldRightUsingFoldLeft(List(1, 2, 3), "0")(_ + _) shouldBe "3210"
  }

  "Zip two lists of ints by adding their elements" should "work for unbalanced lists regarding their length" in {
    List.zipByAddingInts(List(1, 2, 3, 4), List(100, 200)) shouldBe List(101, 202)
  }

  "Using zip with to combine two lists of ints by adding their elements" should "work for the example" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
  }

  "Checking a list for subsequences" should "work for the example" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true
  }

  it should "work for edge cases" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4)) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List()) shouldBe true
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 1)) shouldBe false
    List.hasSubsequence(List(1, 2, 3, 4), List(5)) shouldBe false
  }
}
