package sequential_ad

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

class Seq_ExpressionsTests_Reverse_Mode extends AnyFunSuite with BeforeAndAfterEach {
  override def beforeEach(): Unit = {
    PartialDerivativeOf.grads.clear()
    ValueOf.values.clear()
    super.beforeEach() // To be stackable, must call super.beforeEach()
  }

  test("check differentiation of x * x at x = 2") {
    val expString = "x * x"
    val varNames = List("x")
    val varValues = List(2.0)
    val varAssn = varNames.zip(varValues).toMap
    
    val result = AutoDiff.reverseMode(expString, varAssn)
    assert(result("x") == 4.0)
  }

  test("check differentiation of x + y at x = 1, y = 2") {
    val expString = "x + y"
    val varNames = List("x", "y")
    val varValues = List(1.0, 2.0)
    val varAssn = varNames.zip(varValues).toMap

    val reverseResult = AutoDiff.reverseMode(expString, varAssn)
    assert(reverseResult("x") == 1.0)
    assert(reverseResult("y") == 1.0)
  }

  test("check differentiation of x * y at x = 2, y = 3") {
    val expString = "x * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap


    val reverseResult = AutoDiff.reverseMode(expString, varAssn)
    assert(reverseResult("x") == 3.0)
    assert(reverseResult("y") == 2.0)
  }

  test("check differentiation of sin(x) at x = pi/2") {
    val expString = "sin(x)"
    val varNames = List("x")
    val varValues = List(Math.PI / 2)
    val varAssn = varNames.zip(varValues).toMap

    val reverseResult = AutoDiff.reverseMode(expString, varAssn)
    assert(Math.abs(reverseResult("x")) < 1e-6) // Should be close to 0
  }

  test("check differentiation of cos(x) at x = 0") {
    val expString = "cos(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val reverseResult = AutoDiff.reverseMode(expString, varAssn)
    assert(reverseResult("x") == 0.0) // Derivative of cos(x) at x = 0 is 0
  }

  test("check differentiation of ln(x) at x = 1") {
    val expString = "ln(x)"
    val varNames = List("x")
    val varValues = List(1.0)
    val varAssn = varNames.zip(varValues).toMap

    val reverseResult = AutoDiff.reverseMode(expString, varAssn)
    assert(reverseResult("x") == 1.0) // Derivative of ln(x) at x = 1 is 1
  }

  test("check differentiation of x^y at x = 2, y = 3") {
    val expString = "x^y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap

    val reverseResult = AutoDiff.reverseMode(expString, varAssn)
    assert(reverseResult("x") == 3 * Math.pow(2.0, 2.0)) // 3 * 2^(3-1) = 3 * 4 = 12
    assert(reverseResult("y") == Math.pow(2.0, 3.0) * Math.log(2.0)) // 2^3 * ln(2)
  }

  test("check differentiation of x * (x + y) + y * y at x = 2, y = 3") {
    val expString = "x * (x + y) + y * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap

    val reverseResult = AutoDiff.reverseMode(expString, varAssn)
    assert(reverseResult("x") == 7.0)
    assert(reverseResult("y") == 8.0)
  }
}


