package parallel_ad

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite
import parallel_ad.{
  Par_AutoDiff,
  Par_Parser,
  PartialDerivativeOf,
  Par_Process,
  ValueAndPartial
}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class Par_ExpressionsTests extends AnyFunSuite with BeforeAndAfterEach {
  override def beforeEach(): Unit = {
    PartialDerivativeOf.grads.clear()
    ValueOf.values.clear()
    super.beforeEach() // To be stackable, must call super.beforeEach()
    
  }

  test("basic foward mode") {
    val expString = "x * (x + y) + y * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap
    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Par_Process.eval(exp, varAssn)
    val xPartial = Par_AutoDiff.forwardMode(exp, varAssn, "x")
    val yPartial = Par_AutoDiff.forwardMode(exp, varAssn, "y")
    assert(evaluated == 19.0)
    assert(xPartial == ValueAndPartial(19.0, 7.0))
    assert(yPartial == ValueAndPartial(19.0, 8.0))
  }

  test("basic reverse mode") {
    val expString = "x * (x + y) + y * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    Par_AutoDiff.reverseMode(exp, varAssn)
    val xVar =
      exp.findVar("x").getOrElse(throw new Exception("Variable not found"))
    val yVar =
      exp.findVar("y").getOrElse(throw new Exception("Variable not found"))
    val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
    val yGrad = PartialDerivativeOf.grads.getOrElse("y", 0.0)
    println(xGrad)
    println(yGrad)
    assert(xGrad == 7.0)
    assert(yGrad == 8.0)

  }

  test("sin forward mode") {
    val expString = "sin(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Par_Process.eval(exp, varAssn)
    val xPartial = Par_AutoDiff.forwardMode(exp, varAssn, "x")
    assert(evaluated == 0.0)
    assert(xPartial == ValueAndPartial(0.0, 1.0))
  }

  test("sin reverse mode") {
    val expString = "sin(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    Par_AutoDiff.reverseMode(exp, varAssn)
    val xVar =
      exp.findVar("x").getOrElse(throw new Exception("Variable not found"))
    val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
    assert(xGrad == 1.0)
  }

  test("cos forward mode") {
    val expString = "cos(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Par_Process.eval(exp, varAssn)
    val xPartial = Par_AutoDiff.forwardMode(exp, varAssn, "x")

    assert(evaluated == 1.0)
    assert(xPartial == ValueAndPartial(1.0, 0.0))
  }

  test("cos reverse mode") {
    val expString = "cos(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    Par_AutoDiff.reverseMode(exp, varAssn)
    val xVar =
      exp.findVar("x").getOrElse(throw new Exception("Variable not found"))
    val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
    assert(xGrad == 0.0)
  }

  test("tan forward mode") {
    val expString = "tan(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Par_Process.eval(exp, varAssn)
    val xPartial = Par_AutoDiff.forwardMode(exp, varAssn, "x")
    assert(evaluated == 0.0)
    assert(xPartial == ValueAndPartial(0.0, 1.0))
  }

  test("tan reverse mode") {
    val expString = "tan(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    Par_AutoDiff.reverseMode(exp, varAssn)
    val xVar =
      exp.findVar("x").getOrElse(throw new Exception("Variable not found"))
    val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
    assert(xGrad == 1.0)
  }

  test("mixed trig forward mode") {
    val expString = "sin(x) + cos(x) + tan(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Par_Process.eval(exp, varAssn)
    val xPartial = Par_AutoDiff.forwardMode(exp, varAssn, "x")
    assert(evaluated == 1.0)
    assert(xPartial == ValueAndPartial(1.0, 2.0))
  }

  test("mixed trig reverse mode") {
    val expString = "sin(x) + cos(x) + tan(x)"
    val varNames = List("x")
    val varValues = List(0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    Par_AutoDiff.reverseMode(exp, varAssn)
    val xVar =
      exp.findVar("x").getOrElse(throw new Exception("Variable not found"))
    val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
    assert(xGrad == 2.0)

  }

  test("mixed trig forward mode with multiple variables") {
    val expString = "sin(x) + cos(y) + tan(x)"
    val varNames = List("x", "y")
    val varValues = List(0.0, 0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Par_Process.eval(exp, varAssn)
    val xPartial = Par_AutoDiff.forwardMode(exp, varAssn, "x")
    val yPartial = Par_AutoDiff.forwardMode(exp, varAssn, "y")
    assert(evaluated == 1.0)
    assert(xPartial == ValueAndPartial(1.0, 2.0))
    assert(yPartial == ValueAndPartial(1.0, 0))
  }

  test("mixed trig reverse mode with multiple variables") {
    val expString = "sin(x) + cos(y) + tan(x)"
    val varNames = List("x", "y")
    val varValues = List(0.0, 0.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    Par_AutoDiff.reverseMode(exp, varAssn)
    val xVar =
      exp.findVar("x").getOrElse(throw new Exception("Variable not found"))
    val yVar =
      exp.findVar("y").getOrElse(throw new Exception("Variable not found"))
    val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
    val yGrad = PartialDerivativeOf.grads.getOrElse("y", 0.0)
    assert(xGrad == 2.0)
    assert(yGrad == 0)
  }

  test("ln forward mode") {
    val expString = "ln(x)"
    val varNames = List("x")
    val varValues = List(1.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Par_Process.eval(exp, varAssn)
    val xPartial = Par_AutoDiff.forwardMode(exp, varAssn, "x")
    assert(evaluated == 0.0)
    assert(xPartial == ValueAndPartial(0.0, 1.0))
  }

  test("ln reverse mode") {
    val expString = "ln(x)"
    val varNames = List("x")
    val varValues = List(1.0)
    val varAssn = varNames.zip(varValues).toMap

    val exp =
      Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    Par_AutoDiff.reverseMode(exp, varAssn)
    val xVar =
      exp.findVar("x").getOrElse(throw new Exception("Variable not found"))
    val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
    assert(xGrad == 1.0)
  }

}
