package parallel_ad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Par_AutoDiff {
  def reset(): Unit = {

    PartialDerivativeOf.grads.clear()
  }

  def forwardMode(
      expString: String,
      varAssn: Map[String, Double]
  ): Map[String, Double] = {
    val expr =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val futures = varAssn.keys.map { variable =>
      expr.forward(varAssn, variable).map(vp => (variable, vp.partial))
    }
    val resultFut = Future.sequence(futures).map(_.toMap)
    Await.result(resultFut, Duration.Inf)
  }
  def reverseMode(
      expString: String,
      varAssn: Map[String, Double]
  ): Map[String, Double] = {
    val expr =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val backwardFuture = expr.backward(1, varAssn)
    val resultFut = backwardFuture.map { _ =>
      varAssn.keys.map { key =>
        val grad = PartialDerivativeOf.grads.getOrElse(key, 0.0)
        (key, grad)
      }.toMap
    }
    Await.result(resultFut, Duration.Inf)
  }

}
