package parallel_ad

object Par_AutoDiff{
  def forwardMode(e: Par_Expression, varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    e.forward(varAssn, variable)
  }

  def reverseMode(expr:Par_Expression, varAssn: Map[String, Double]): Unit = {
    // Initialize the values of the expressions
    val evaluated = Par_Process.eval(expr, varAssn)

//    println("Value of the expression: " + evaluated)
    // Start backward pass from the output
    expr.backward(1, varAssn)
  }
}