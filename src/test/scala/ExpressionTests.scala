import solver.{AutoDiff, Parser, PartialDerivativeOf, Process, ValueAndPartial}

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class ExpressionTests extends munit.FunSuite:
  test("basic foward mode") {
    val expString = "x * (x + y) + y * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap
    val exp = Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    val evaluated = Process.eval(exp, varAssn)
    val xPartial = AutoDiff.forwardMode(exp, varAssn, "x")
    val yPartial = AutoDiff.forwardMode(exp, varAssn, "y")
    assertEquals(evaluated, 19.0)
    assertEquals(xPartial, ValueAndPartial(19.0, 7.0))
    assertEquals(yPartial, ValueAndPartial(19.0, 8.0))
  }
  
  test("basic reverse mode"){
    val expString = "x * (x + y) + y * y"
    val varNames = List("x", "y")
    val varValues = List(2.0, 3.0)
    val varAssn = varNames.zip(varValues).toMap
    
    val exp = Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    AutoDiff.reverseMode(exp, varAssn)
    val xVar = exp.findVar("x").getOrElse(throw new Exception("Variable not found"))
    val yVar = exp.findVar("y").getOrElse(throw new Exception("Variable not found"))
    val xGrad = PartialDerivativeOf.grads.getOrElse("x", 0.0)
    val yGrad = PartialDerivativeOf.grads.getOrElse("y", 0.0)
    assertEquals(xGrad, 7.0)
    assertEquals(yGrad, 8.0)
  }
  
  


