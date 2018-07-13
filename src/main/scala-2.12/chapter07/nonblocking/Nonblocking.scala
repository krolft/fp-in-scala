package chapter07.nonblocking

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {

  // a future for now is only an interface
  // implementations are only possible within this package
  sealed trait Future[+A] {

    // calling the method 'apply' makes it possible to use instances of Future as if it were a function, e.g.:
    // val f: Future[A] = ???
    // f(a => ???)
    // this is only syntactical sugar of course

    // apply will be called when A has been calculated
    // the callback is supposed to have side effects since it doesn't return a value

    // also the apply method itself doesn't return a value which makes sense:
    // if we work with side effects, let the implementor decide how to do it
    private[nonblocking] def apply(cb: A => Unit): Unit
  }

  // Par is a function that when you pass it an executor service to it
  // - will give you a future in return
  //
  // Currently there are only two ways to create a Par:
  // - 'unit' will when passed an executor service
  //      ignore it and instantly call the callback method with the value passed to it
  // - 'fork' will create a new Par and
  //      when passed an executor service
  //      submit the passed Par as a task to the executor service
  //
  // Par still looks the same:
  // - on the left side it is: we still need an executor service to work with threads
  // - BUT the right side is different: a future can't be used to retrieve a value from ...
  type Par[+A] = ExecutorService => Future[A]

  // ... this is why run needs to implement retrieving the resulting value
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    Actor.clearStatistic()

    // AtomicReference is usually used as a more lightweight / optimistic locking alternative to 'synchronized'
    // we don't make use of its 'compareAndSet' method, so I'm not sure if we really need it
    // on the other hand, ref.set(a) will be called from another thread and we have a nice container for the value
    val ref = new AtomicReference[A]

    // latch is the vehicle to block until the callback method that is given to apply has been called
    val latch = new CountDownLatch(1)

    // same as 'p(es).apply { a => ref.set(a); latch.countDown() }'
    // p(es) returns a future: 'type Par[+A] = ExecutorService => Future[A]'
    // we use the future to register a callback
    // as soon as a "is ready", the callback will be called, a will be stored and the latch decremented
    // which will unblock 'latch.await'
    p(es) { a => ref.set(a); latch.countDown() }

    // here we block until the callback method is called
    // when the callback has been called, we stop blocking
    latch.await()

    // and return the value that has been stored within the callback
    ref.get
  }

  // unit takes a value A and turns it in an Par[A]
  def unit[A](a: A): Par[A] = {
    // we don't need the executor service since we only want to elevate a value we already have to a Par
    _ =>
      new Future[A] {
        // we just call the callback method instantly
        override private[nonblocking] def apply(cb: A => Unit): Unit = cb(a)
      }
  }

  // fork creates a Par function that
  // - when called with an executor service
  // - will submit the passed-in Par to the executor service
  //
  // fork takes something that should be calculated in parallel and
  // creates a new future which callback is called when the executor service eventually executed the task
  // and the task finished
  def fork[A](pa: => Par[A]): Par[A] =
    es => new Future[A] {
      override private[nonblocking] def apply(cb: A => Unit): Unit = {
        // the executor service 'es' is passed on to eval
        //
        // 'pa(es)(cb)' is returning Unit and is running in parallel
        //    since it is passed in as the lazy parameter r to eval,
        //    it is called only when the executor service executing able to process a task
        //    furthermore the processing is done within the scope of the executor service
        //    in whatever thread it chooses
        eval(es)(pa(es)(cb))
      }
    }

  // eval takes an executor service and is actually using it to submit tasks
  // it also takes a lazy parameter r of type Unit
  def eval[A](es: ExecutorService)(r: => Unit): Unit = {
    // a anonymous implementation of callable is passed to the executor service
    // as soon as the executor service gets to it, 'call' is called which in turn
    // leads to the evaluation of r
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }
    }

  // specialized version of `map`
  def map[A,B](p: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit =
        p(es)(a => eval(es) { cb(f(a)) })
    }

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  def parMap[A,B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalanced(as.map(asyncF(f)))
}

