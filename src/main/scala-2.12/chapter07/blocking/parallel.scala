package chapter07.blocking

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.TimeUnit

/*
class ExecutorService {
  def submit[A](a: Callable[A]): Future[A] = ???
}

trait Callable[A] {
  def call: A
}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}
*/

private case class UnitFuture[A](value: A) extends Future[A] {
  override def get: A = value

  override def get(timeout: Long, unit: TimeUnit): A = get

  override def cancel(evenIfRunning: Boolean): Boolean = false

  override def isDone: Boolean = true

  override def isCancelled: Boolean = false
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  def fork[A](a: Par[A]): Par[A] = es =>
    es.submit(new Callable[A] {
      override def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => {
    val a = pa(es)
    val b = pb(es)
    UnitFuture(f(a.get, b.get))
  }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortParCustom(pas: Par[List[Int]]): Par[List[Int]] =
    map2(pas, unit(()))((list, _) => list.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(pas: Par[List[Int]]): Par[List[Int]] =
    map(pas)(_.sorted)

  def sequence[A](lpa: List[Par[A]]): Par[List[A]] =
    lpa.foldRight[Par[List[A]]](unit(List()))((pa, pla) => map2(pa, pla)(_ :: _))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    val pbs: List[Par[B]] = as.map(asyncF(f(_)))
    sequence(pbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldRight(unit(List[A]()))(
      (a, pas) =>
        map2(asyncF(f)(a), pas) {
          (result, as) => if (result) a :: as else as
        }
    )

  def parFilterFromBook[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pas = as.map(asyncF((a: A) => if (f(a)) a :: Nil else Nil)(_))
    map(sequence(pas))(_.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (a, b) = ints.splitAt(ints.length / 2)
      map2(fork(sum(a)), fork(sum(b)))(_ + _)
    }

  def max(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      unit(ints.headOption getOrElse Int.MinValue)
    else {
      val (a, b) = ints.splitAt(ints.length / 2)
      map2(fork(sum(a)), fork(sum(b)))(Math.max)
    }

  def reduce[A](as: IndexedSeq[A], zero: A)(f: (A, A) => A): Par[A] =
    if (as.length <= 1)
      unit(as.headOption getOrElse zero)
    else {
      val (a, b) = as.splitAt(as.length / 2)
      map2(fork(reduce(a, zero)(f)), fork(reduce(b, zero)(f)))(f)
    }

  def maxUsingReduce(ints: IndexedSeq[Int]): Par[Int] =
    reduce(ints, Int.MinValue)(Math.max)

  def sumUsingReduce(ints: IndexedSeq[Int]): Par[Int] =
    reduce(ints, 0)(_ + _)

  def countWords(lines: IndexedSeq[String]): Par[Int] =
    if (lines.length <= 1)
      unit(lines.headOption.map(_.split("\\s+").length) getOrElse 0)
    else {
      val (a, b) = lines.splitAt(lines.length / 2)
      map2(fork(countWords(a)), fork(countWords(b)))(_ + _)
    }

  def mapReduce[A, B](as: IndexedSeq[A], zero: B)(m: A => B)(r: (B, B) => B): Par[B] =
    if (as.length <= 1)
      unit(as.headOption.map(m) getOrElse zero)
    else {
      val (a, b) = as.splitAt(as.length / 2)
      map2(fork(mapReduce(a, zero)(m)(r)), fork(mapReduce(b, zero)(m)(r)))(r)
    }

  def countWordsUsingMapReduce(lines: IndexedSeq[String]): Par[Int] =
    mapReduce(lines, 0)(_.split("\\s+").length)(_ + _)

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {

    def curry[E, F, G, H](j: (E, F, G) => H): (E, F) => G => H =
      (e, f) => g => j(e, f, g)

    val parOfCToD: Par[C => D] = map2[A, B, C => D](pa, pb)((a, b) => curry(f)(a, b))
    map2(pc, parOfCToD)((c, cToD) => cToD(c))
  }

  def equal[A](es: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(es).get(1, TimeUnit.SECONDS) == p2(es).get(1, TimeUnit.SECONDS)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) else f(es)

  def choiceN[A](i: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      choices(run(es)(i).get())(es)

  def choiceInTermsOfChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(c => if (c) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es =>
      choices(run(es)(key).get)(es)

  // based on the result of a Par we want to select
  def chooser[A, B](key: Par[A])(selector: A => Par[B]): Par[B] =
    es =>
      selector(run(es)(key).get)(es)

  def choiceNInTermsOfChooser[A](i: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(i)(i => choices(i))

  def join[A](ppa: Par[Par[A]]): Par[A] = {
    es =>
      // implementation from book:
      // 'run(es)(run(es)(ppa).get())'
      // since run is implemented like this:
      //   def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)
      // it is semantically the same as my implementation:
      run(es)(ppa).get()(es)
  }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es =>
      f(run(es)(pa).get)(es)

  def flatMapUsingJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(a => f(a)))

  def joinUsingFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(pa => pa)

}
