package sequential_ad

object Main {
  def main(args: Array[String]): Unit = {
    val expString = "ln(x) + x * y - sin(y)"
    val varNames = List("x", "y")
    val varValues = List(2.0, 5.0)
    val varAssn = varNames.zip(varValues).toMap
    val exp = Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Process.eval(exp, varAssn)
    val xPartial = AutoDiff.forwardMode(exp, varAssn, "x")
    val yPartial = AutoDiff.forwardMode(exp, varAssn, "y")

    println(s"Evaluated: $evaluated")
    println(s"xPartial: ${xPartial.toList}")
    println(s"yPartial: ${yPartial.toList}")



  }
}

