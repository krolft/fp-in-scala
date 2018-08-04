package chapter07.nonblocking

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger

import Nonblocking._
import org.scalatest.{FlatSpec, Matchers}

class NonblockingTest extends FlatSpec with Matchers {
  "Running a Par" should "work for unit" in {
    Nonblocking.run(Executors.newCachedThreadPool())(unit(3)) shouldBe 3
  }

  it should "work for simple forked units" in {
    Nonblocking.run(Executors.newCachedThreadPool())(fork {
      println("going to sleep...")
      Thread.sleep(2000)
      unit(3)
    }) shouldBe 3
  }

  it should "wait until run to calculate forked units" in {
    val aIntEager = new AtomicInteger(0)
    val parEager = unit(aIntEager.incrementAndGet())
    aIntEager.get() shouldBe 1

    val aIntForked = new AtomicInteger(0)
    val parForked = fork(unit(aIntForked.incrementAndGet()))
    aIntForked.get() shouldBe 0

    Nonblocking.run(Executors.newCachedThreadPool())(parEager) shouldBe 1
    Nonblocking.run(Executors.newCachedThreadPool())(parForked) shouldBe 1
  }

  "The map2 implementation" should "work for fixed size thread pools" in {
    val p = parMap(List.range(1, 10000))(math.sqrt(_))
    Nonblocking.run(Executors.newFixedThreadPool(2))(p).sum > 1.0 shouldBe true
    println(Actor.statistic())
  }
}
