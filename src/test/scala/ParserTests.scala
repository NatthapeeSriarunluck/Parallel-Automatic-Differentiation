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



