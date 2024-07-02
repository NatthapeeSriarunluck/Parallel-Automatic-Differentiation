class ValueAndPartial(var value: Double, var partial: Double) {
  def toList(): List[Double] = List(value, partial)
}
