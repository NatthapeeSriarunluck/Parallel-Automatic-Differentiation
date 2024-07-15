package parser_test

import org.scalatest.funsuite.AnyFunSuite
import parallel_ad.Par_Parser
class Par_ParserTests extends AnyFunSuite:

  test("sin expression" ) {
    val expString = "sin(x)"
    val exp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Sin(Var(x))")
  }

  test("cos expression") {
    val expString = "cos(x)"
    val exp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Cos(Var(x))")
  }

  test("tan expression") {

    val expString = "tan(x)"
    val exp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Tan(Var(x))")
  }

  test("mixed trig expression") {
    val expString = "sin(x) + cos(x) + tan(x)"
    val exp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString ==   "Sum(Sum(Sin(Var(x)), Cos(Var(x))), Tan(Var(x)))")
  }

  test("ln expression") {
    val expString = "ln(x)"
    val exp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Ln(Var(x))")
  }

  test("power expression") {
    val expString = "x^2"
    val exp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Power(Var(x),Constant(2.0))")
  }

  test("xy") {
    val expString = "xy"
    val exp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Prod(Var(x),Var(y))")
  }



