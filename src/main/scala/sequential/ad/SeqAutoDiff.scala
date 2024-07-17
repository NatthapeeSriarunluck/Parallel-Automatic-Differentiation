package sequential.ad

object SeqAutoDiff {

  def reset(): Unit = {
    PartialDerivativeOf.grads.clear()
  }
  def forwardMode(
      expString: String,
      varAssn: Map[String, Double]
  ): Map[String, Double] = {
    val expr =
      SeqParser(expString).getOrElse(throw new Exception("Invalid expression"))
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
      SeqParser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = SeqProcess.eval(expr, varAssn)
    val reverseResult: Unit = expr.reverse(1, varAssn)

    val resMap = varAssn.keys.map { key =>
      val grad = PartialDerivativeOf.grads.getOrElse(key, 0.0)
      (key, grad)
    }.toMap

    resMap
  }
}
