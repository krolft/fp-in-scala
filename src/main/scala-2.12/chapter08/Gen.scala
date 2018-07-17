package chapter08

import chapter06.{RNG, RNG_USING_STATE_ACTION, StateAction}

case class Gen[A](sample: StateAction[RNG, A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen[Int](RNG_USING_STATE_ACTION.intBetween(start, stopExclusive))

  def unit[A](a: => A): Gen[A] =
    Gen[A](RNG_USING_STATE_ACTION.unit(a))

  def boolean: Gen[Boolean] =
    Gen[Boolean](RNG_USING_STATE_ACTION.boolean)

  def listOf[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen(StateAction.sequence(List.fill(n)(gen.sample)))
}
