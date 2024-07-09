package solver

object Main {
  def main(args: Array[String]): Unit = {
    val expString = "x * (x + y) + y * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap

    try {

      val exp = Parser(expString).getOrElse(throw new Exception("Invalid expression"))
      // Forward Mode
      println("Forward Mode")

      val evaluated = Process.eval(exp, varAssn)
      println(s"The value of the expression at the point $varAssn is $evaluated")

      val xPartial = AutoDiff.forwardMode(exp, varAssn, "x")
      val yPartial = AutoDiff.forwardMode(exp, varAssn, "y")
      println(s"The partial derivative with respect to x at the point $varAssn is $xPartial")
      println(s"The partial derivative with respect to y at the point $varAssn is $yPartial")

      // Reverse Mode
      println("Reverse Mode")
      val exp2 = Parser(expString).getOrElse(throw new Exception("Invalid expression"))
      AutoDiff.reverseMode(exp2, varAssn)
      val xVar = exp2.findVar("x").getOrElse(throw new Exception("Variable not found"))
      val yVar = exp2.findVar("y").getOrElse(throw new Exception("Variable not found"))
      val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
      val yGrad = PartialDerivativeOf.grads.getOrElse("y", 0.0)
      println(s"The partial derivative with respect to x at the point $varAssn is $xGrad")
      println(s"The partial derivative with respect to y at the point $varAssn is $yGrad")
    }
    catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}

