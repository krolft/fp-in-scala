package chapter08

trait Prop {
  import chapter08.Prop._

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: PropBoolean): PropBoolean = {
    ???
  }
}
object Prop {
  type FailedCase = String
  type SuccessCount = Int
}
