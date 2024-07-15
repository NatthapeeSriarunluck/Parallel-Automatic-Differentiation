package parallel_ad

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.Await
import scala.concurrent.duration.*

class Par_ExpressionsTests_Forward_Mode extends AnyFunSuite with BeforeAndAfterEach {
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

    val forwardResult = Par_AutoDiff.forwardMode(expString, varAssn)
    assert(forwardResult("x") == 4.0)
  }

  test("check differentiation of x + y at x = 1, y = 2") {
    val expString = "x + y"
    val varNames = List("x", "y")
    val varValues = List(1.0, 2.0)
    val varAssn = varNames.zip(varValues).toMap

    val result = Par_AutoDiff.forwardMode(expString, varAssn)
    assert(result("x") == 1.0)
    assert(result("y") == 1.0)

  }

  test("check differentiation of x * y at x = 2, y = 3") {
    val expString = "x * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap

    val result = Par_AutoDiff.forwardMode(expString, varAssn)
    assert(result("x") == 3.0)
    assert(result("y") == 2.0)
  }

  test("check differentiation of sin(x) at x = pi/2") {
    val expString = "sin(x)"
    val varNames = List("x")
    val varValues = List(Math.PI / 2)
    val varAssn = varNames.zip(varValues).toMap

    val forwardResult = Par_AutoDiff.forwardMode(expString, varAssn)
    assert(Math.abs(forwardResult("x")) < 1e-6) // Should be close to 0
  }

  test("check differentiation of cos(x) at x = 0") {
    val expString = "cos(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val forwardResult = Par_AutoDiff.forwardMode(expString, varAssn)
    assert(forwardResult("x") == 0.0)

  }

  test("check differentiation of ln(x) at x = 1") {
    val expString = "ln(x)"
    val varNames = List("x")
    val varValues = List(1.0)
    val varAssn = varNames.zip(varValues).toMap

    val forwardResult = Par_AutoDiff.forwardMode(expString, varAssn)
    assert(forwardResult("x") == 1.0)
  }

  test("check differentiation of x^y at x = 2, y = 3") {
    val expString = "x^y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap

    val forwardResult = Par_AutoDiff.forwardMode(expString, varAssn)
    assert(forwardResult("x") == 12.0)
    assert(forwardResult("y") == Math.pow(2.0, 3.0) * Math.log(2.0)) // 2^3 * ln(2)
  }

  test("check differentiation of x * (x + y) + y * y at x = 2, y = 3") {
    val expString = "x * (x + y) + y * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap

    val forwardResultX = Par_AutoDiff.forwardMode(expString, varAssn)
    assert(forwardResultX("x") == 7.0)
    assert(forwardResultX("y") == 8.0)

  }
}


