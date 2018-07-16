package chapter08

trait PropBoolean {
  def check: Boolean

  // if we only care about any of the chained props to failing,
  // we can ignore all remaining props if one fails
  def &&(p: PropBoolean): PropBoolean = {
    if (!check) this
    else p
  }
}
