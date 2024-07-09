package solver

object AutoDiff{
  def forwardMode(e: Expression, varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    e.evaluateAndDerive(varAssn, variable)
  }

  def reverseMode(expr: Expression, varAssn: Map[String, Double]): Unit = {
    // Initialize the values of the expressions
    val evaluated = Process.eval(expr, varAssn)

    println("Value of the expression: " + evaluated)
    // Start backward pass from the output
    expr.derive(1, varAssn)
  }

}