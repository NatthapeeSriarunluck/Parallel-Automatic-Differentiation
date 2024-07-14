package sequential_ad

object AutoDiff {
  def forwardMode(
      e: Expression,
      varAssn: Map[String, Double],
      variable: String
  ): ValueAndPartial = {
    e.forward(varAssn, variable)
  }

  def reverseMode(expr: Expression, varAssn: Map[String, Double]): Unit = {
    // Initialize the values of the expressions
    val evaluated = Process.eval(expr, varAssn)

//    println("Value of the expression: " + evaluated)
    // Start backward pass from the output
    expr.backward(1, varAssn)
  }
}
