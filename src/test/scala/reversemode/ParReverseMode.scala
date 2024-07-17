package reversemode

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import parallel.ad.{ParAutoDiff, PartialDerivativeOf, ValueOf}

class ParReverseMode extends AnyFunSuite with BeforeAndAfterEach {
  override def beforeEach(): Unit = {
    PartialDerivativeOf.grads.clear()
    ValueOf.values.clear()
    super.beforeEach() // To be stackable, must call super.beforeEach()
  }

  def testReverseModeAutoDiffs(
      testName: String,
      expression: String,
      varNames: List[String],
      varValues: List[Double],
      expected: Map[String, Double]
  ): Unit = {
    val varAssn = varNames.zip(varValues).toMap
    test(s"$testName (Par_AutoDiff)") {
      val result = ParAutoDiff.reverseMode(expression, varAssn)
      expected.foreach { case (variable, value) =>
        assert(result(variable) == value)
      }
    }
  }

  testReverseModeAutoDiffs(
    "check differentiation of x * x at x = 2",
    "x * x",
    List("x"),
    List(2.0),
    Map("x" -> 4.0)
  )
  testReverseModeAutoDiffs(
    "check differentiation of x + y at x = 1, y = 2",
    "x + y",
    List("x", "y"),
    List(1.0, 2.0),
    Map("x" -> 1.0, "y" -> 1.0)
  )
  testReverseModeAutoDiffs(
    "check differentiation of x * y at x = 2, y = 3",
    "x * y",
    List("x", "y"),
    List(2.0, 3.0),
    Map("x" -> 3.0, "y" -> 2.0)
  )
  testReverseModeAutoDiffs(
    "check differentiation of cos(x) at x = 0",
    "cos(x)",
    List("x"),
    List(0.0),
    Map("x" -> 0.0)
  )
  testReverseModeAutoDiffs(
    "check differentiation of ln(x) at x = 1",
    "ln(x)",
    List("x"),
    List(1.0),
    Map("x" -> 1.0)
  )
  testReverseModeAutoDiffs(
    "check differentiation of x^y at x = 2, y = 3",
    "x^y",
    List("x", "y"),
    List(2.0, 3.0),
    Map("x" -> 12.0, "y" -> (Math.pow(2.0, 3.0) * Math.log(2.0)))
  )
  testReverseModeAutoDiffs(
    "check differentiation of x * (x + y) + y * y at x = 2, y = 3",
    "x * (x + y) + y * y",
    List("x", "y"),
    List(2.0, 3.0),
    Map("x" -> 7.0, "y" -> 8.0)
  )
  testReverseModeAutoDiffs(
    "check differentiation of x^2 + 2xy at x = 2, y = 3",
    "x^2 + 2 * x * y",
    List("x", "y"),
    List(2.0, 3.0),
    Map("x" -> 10.0, "y" -> 4.0)
  )


}
