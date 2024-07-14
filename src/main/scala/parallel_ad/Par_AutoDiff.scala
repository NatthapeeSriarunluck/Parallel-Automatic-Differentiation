package parallel_ad

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Par_AutoDiff {
  def forwardMode(
      e: Par_Expression,
      varAssn: Map[String, Double],
      variable: String
  ): ValueAndPartial = {
    e.forward(varAssn, variable)
  }

  def reverseMode(
      expString : String,
      varAssn: Map[String, Double]
  ): Future[Map[String, Double]] = {
    val expr = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val backwardFuture = expr.backward(1, varAssn)
    backwardFuture.map { _ =>
      varAssn.keys.map { key =>
        val grad = PartialDerivativeOf.grads.getOrElse(key, 0.0)
        (key, grad)
      }.toMap
    }

    // ...
  }
}
