
package parser_test

import org.scalatest.funsuite.AnyFunSuite
import sequential_ad.Parser
import parallel_ad.Par_Parser

class ParserTests extends AnyFunSuite {

  def testParsers(testName: String, expression: String, expected: String): Unit = {
    test(s"$testName (sequential)") {
      val exp = Parser(expression).getOrElse(throw new Exception("Invalid expression"))
      assert(exp.toString == expected)
    }

    test(s"$testName (parallel)") {
      val exp = Par_Parser(expression).getOrElse(throw new Exception("Invalid expression"))
      assert(exp.toString == expected)
    }
  }

  testParsers("sin expression", "sin(x)", "Sin(Var(x))")
  testParsers("cos expression", "cos(x)", "Cos(Var(x))")
  testParsers("tan expression", "tan(x)", "Tan(Var(x))")
  testParsers("mixed trig expression", "sin(x) + cos(x) + tan(x)", "Sum(Sum(Sin(Var(x)),Cos(Var(x))),Tan(Var(x)))")
  testParsers("ln expression", "ln(x)", "Ln(Var(x))")
  testParsers("power expression", "x^2", "Power(Var(x),Constant(2.0))")
  testParsers("xy", "xy", "Prod(Var(x),Var(y))")
  testParsers("x^2 + 2xy", "x^2 + 2xy", "Sum(Power(Var(x),Constant(2.0)),Prod(Constant(2.0),Prod(Var(x),Var(y))))")
}
