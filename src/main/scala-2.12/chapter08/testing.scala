package chapter08

trait Prop {
  def check: Boolean

  // if we only care about any of the chained props to fail,
  // we can ignore all remaining props if one fails
  def &&(p: Prop): Prop = {
    if (!check) this
    else p
  }
}

