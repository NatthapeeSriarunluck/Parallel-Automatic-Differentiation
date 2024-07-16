package sequential_ad

object AutoDiff {

  def reset(): Unit = {
    PartialDerivativeOf.grads.clear()
  }
  def forwardMode(
      expString: String,
      varAssn: Map[String, Double]
  ): Map[String, Double] = {
    val expr =
      Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    varAssn.keys.map { variable =>
      val vp = expr.forward(varAssn, variable)
      (variable, vp.partial)
    }.toMap
  }

  def reverseMode(
      expString: String,
      varAssn: Map[String, Double]
  ): Map[String, Double] = {

    val expr =
      Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Process.eval(expr, varAssn)
    val backwardResult: Unit = expr.backward(1, varAssn)

    val resMap = varAssn.keys.map { key =>
      val grad = PartialDerivativeOf.grads.getOrElse(key, 0.0)
      (key, grad)
    }.toMap

    resMap
  }
}
