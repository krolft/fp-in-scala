package chapter07

import java.util.concurrent.TimeUnit

object parallel {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.size / 2)
      sum(l) + sum(r)
    }

  //  def sumWithParType(ints: IndexedSeq[Int]): Int =
  //    if (ints.size <= 1)
  //      ints.headOption getOrElse 0
  //    else {
  //      val (l, r) = ints.splitAt(ints.length / 2)
  //      val sumL: Par[Int] = unit(sumWithParType(l))
  //      val sumR: Par[Int] = unit(sumWithParType(r))
  //      val es = new ExecutorService
  //      run(es)(sumL) + run(es)(sumR)
  //    }

  def sumDeferringCallToGet(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(sumDeferringCallToGet(l), sumDeferringCallToGet(r))(_ + _)
    }

  def sumWithFork(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      lazyUnit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sumWithFork(l)), fork(sumWithFork(r)))(_ + _)
    }

  def max(ints: IndexedSeq[Int]): Par[Option[Int]] =
    if (ints.size <= 1)
      lazyUnit(ints.headOption)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(max(l)), fork(max(r)))(comb(_, _)(math.max))
    }

  def comb[A](optionA: Option[A], optionB: Option[A])(f: (A, A) => A): Option[A] =
    (optionA, optionB) match {
      case (None, Some(b)) => Some(b)
      case (Some(a), None) => Some(a)
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  def words(lines: List[String]): Par[Int] = {
    // sequential approach
    // lines.foldLeft(List[Int]())((list, line) => line.length :: list).sum

    // what could be done in parallel
    // a) calculating the length of lines
    // b) adding up the lengths of lines

    val parOfLineLengths: Par[List[Int]] = parMap(lines)(line => line.length)

    // the following line wouldn't sum in parallel
    // map(parOfLineLengths)(list => list.sum)

    foldLeft(parOfLineLengths)(0)(_ + _)

    // an alternative would be:
    // foldLeft(unit(lines))(0)(_.length + _)
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(()))((list, _) => list.sorted)

  def sortParWithMap(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)
}

class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}
trait Callable[A] {
  // just to be able to retrieve an A in a lazy manner
  def call: A
}

trait Future[A] {
  def get: A

  def get(timeout: Long, unit: TimeUnit): A

  def cancel(evenIfRunning: Boolean): Boolean

  def isDone: Boolean

  def isCancelled: Boolean
}

case class UnitFuture[A](a: A) extends Future[A] {
  override def get: A = a

  override def get(timeout: Long, unit: TimeUnit): A = get

  override def cancel(evenIfRunning: Boolean): Boolean = false

  override def isDone: Boolean = true

  override def isCancelled: Boolean = false
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(new Callable[A] {
      override def call: A = a(es).get
    })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    pas.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def foldLeft[A, B](pas: Par[List[A]])(z: B)(f: (A, B) => B): Par[B] = {
    // TODO ... use eval async and flatMap
    map(pas)(as => as.reduceLeftOption((acc, a) => f(a, acc)).getOrElse(z))
  }

  def parMap[A, B](pas: List[A])(f: A => B): Par[List[B]] = {
    val list = pas.map(evalAsync(f))
    sequence(list)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    // the following is not doing it in parallel but only later in one parallel thread
    // lazyUnit(as.filter(f))

    // we want to use evalAsync for EVERY element of the list
    val listOfParOfListOfA: List[Par[List[A]]] = as.map(evalAsync[A, List[A]](a => if (f(a)) List[A](a) else List[A]()))

    // we want to get from  List[Par[List[A]]] to Par[List[A]]
    // Par[List[List[A]]] is also ok, we can flatten it
    val parOfListOfListA: Par[List[List[A]]] = sequence(listOfParOfListOfA)

    // now we can map over Par using our map to access the list of lists and flatten it
    val parOfListOfA: Par[List[A]] = map(parOfListOfListA)(listOfListA => listOfListA.flatten)

    // we are done!
    parOfListOfA
  }

  def evalAsyncCustom[A, B](f: A => B): A => Par[B] =
    a => es =>
      es.submit(new Callable[B] {
        override def call: B = f(a)
      })

  def evalAsync[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

}
