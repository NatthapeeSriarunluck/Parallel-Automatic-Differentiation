package parser

import org.scalatest.funsuite.AnyFunSuite
import parallel.ad.ParParser
import sequential.ad.SeqParser

class ParserTests extends AnyFunSuite {

  def testParsers(
      testName: String,
      expression: String,
      expected: String
  ): Unit = {
    test(s"$testName (sequential)") {
      val exp =
        SeqParser(expression).getOrElse(throw new Exception("Invalid expression"))
      assert(exp.toString == expected)
    }

    test(s"$testName (parallel)") {
      val exp = ParParser(expression).getOrElse(
        throw new Exception("Invalid expression")
      )
      assert(exp.toString == expected)
    }
  }

  testParsers("sin expression", "sin(x)", "Sin(Var(x))")
  testParsers("cos expression", "cos(x)", "Cos(Var(x))")
  testParsers("tan expression", "tan(x)", "Tan(Var(x))")
  testParsers(
    "mixed trig expression",
    "sin(x) + cos(x) + tan(x)",
    "Sum(Sum(Sin(Var(x)),Cos(Var(x))),Tan(Var(x)))"
  )
  testParsers("ln expression", "ln(x)", "Ln(Var(x))")
  testParsers("power expression", "x^2", "Power(Var(x),Constant(2.0))")
  testParsers("xy", "xy", "Prod(Var(x),Var(y))")
  testParsers(
    "x^2 + 2xy",
    "x^2 + 2xy",
    "Sum(Power(Var(x),Constant(2.0)),Prod(Constant(2.0),Prod(Var(x),Var(y))))"
  )
  testParsers(
    "x^3y^4",
    "x^3y^4",
    "Prod(Power(Var(x),Constant(3.0)),Power(Var(y),Constant(4.0)))"
  )
  testParsers("sec expression", "sec(x)", "Sec(Var(x))")
  testParsers("csc expression", "csc(x)", "Csc(Var(x))")
  testParsers("cot expression", "cot(x)", "Cot(Var(x))")
  testParsers("arcSin expression", "arcsin(x)", "ArcSin(Var(x))")
  testParsers("arcCos expression", "arccos(x)", "ArcCos(Var(x))")
  testParsers("arcTan expression", "arctan(x)", "ArcTan(Var(x))")
  testParsers("arcSec expression", "arcsec(x)", "ArcSec(Var(x))")
  testParsers("arcCsc expression", "arccsc(x)", "ArcCsc(Var(x))")
  testParsers("arcCot expression", "arccot(x)", "ArcCot(Var(x))")

}
