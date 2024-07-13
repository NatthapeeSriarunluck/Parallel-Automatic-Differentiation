import org.scalatest.funsuite.AnyFunSuite
class ParserTests extends AnyFunSuite:

  test("sin expression" ) {
    val expString = "sin(x)"
    val exp = solver.Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Sin(Var(x))")
  }

  test("cos expression") {
    val expString = "cos(x)"
    val exp = solver.Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Cos(Var(x))")
  }

  test("tan expression") {

    val expString = "tan(x)"
    val exp = solver.Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Tan(Var(x))")
  }

  test("mixed trig expression") {
    val expString = "sin(x) + cos(x) + tan(x)"
    val exp = solver.Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString ==   "Sum(Sum(Sin(Var(x)), Cos(Var(x))), Tan(Var(x)))")
  }

  test("ln expression") {
    val expString = "ln(x)"
    val exp = solver.Parser(expString).getOrElse(throw new Exception("Invalid expression"))
    assert(exp.toString == "Ln(Var(x))")
  }



