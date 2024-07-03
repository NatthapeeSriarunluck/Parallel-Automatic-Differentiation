package solver

package solver

object Main {
  def main(args: Array[String]): Unit = {
    val expString = "x * (x + y) + y * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap

    try {
      //      val exp = Parser(expString).getOrElse(throw new Exception("Invalid expression"))
      //      val xPartial = exp.evaluateAndDerive(Map("x" -> varAssn("x"), "y" -> varAssn("y")), "x").partial
      //      val yPartial = exp.evaluateAndDerive(Map("x" -> varAssn("x"), "y" -> varAssn("y")), "y").partial
      //      println(s"The partial derivative with respect to x at the point $varAssn is $xPartial")
      //      println(s"The partial derivative with respect to y at the point $varAssn is $yPartial")

      val exp = Parser(expString).getOrElse(throw new Exception("Invalid expression"))
      val evaluated = Process.eval(exp, varAssn)
      println(s"The expression $expString at the point $varAssn = $evaluated")

      // call the derive method
      exp.derive(1)

      // print the final partial derivative  solved
      println(s"The partial derivative with respect to x at the point $varAssn is ${exp.partial}")
    }

    catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}

