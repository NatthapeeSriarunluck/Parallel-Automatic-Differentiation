package scala
object Solver {

  def solve(expString: String, varName: String): Double = {
    val exp = parseExpression(expString)
    val (value, partial) = exp.evaluateAndDerive(Map(varName -> 1.0))
    value / partial
  }
}

def main(args: Array[String]): Unit = ???
