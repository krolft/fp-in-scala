object ChargeExample {
  def main (args: Array[String]) {
    println(
      coalesce(
        List(Charge("a", 1.0), Charge("b", 2.0), Charge("a", 3.0))
      )
    )
  }

  def coalesce(charges: List[Charge]): List[Charge] = {
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
  }
}

case class Charge(cc: String, amountDouble: Double) {
  def combine(other: Charge): Charge = {
    if (cc == other.cc)
      Charge(cc, amountDouble + other.amountDouble)
    else
      throw new Exception("Can't combine charges of different cards")
  }
}
