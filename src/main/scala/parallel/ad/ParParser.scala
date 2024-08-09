package parallel.ad

import scala.util.parsing.combinator.*

/*
 * The parser object which produces a parsed expression for a given string.
 * As an example,
 *    val e = solver.Parser("x^2 + 3*x - 1")
 */

object ParParser extends JavaTokenParsers {

  def op: Parser[ParExpression] =
    sin | cos | tan | sec | csc | cot | arcsin | arccos | arctan | arcsec | arccsc | arccot | ln | exp | const | variable | ("(" ~> expr <~ ")")

  def expr: Parser[ParExpression] = term ~ rep(("+" | "-") ~ term) ^^ {
    case term ~ Nil => term
    case term ~ repTerms =>
      repTerms.foldLeft(term) {
        case (termSoFar, "+" ~ nextTerm) => Sum(termSoFar, nextTerm)
        case (termSoFar, "-" ~ nextTerm) =>
          Sum(termSoFar, Prod(Constant(-1), nextTerm))
      }
  }

  def term: Parser[ParExpression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case factor ~ Nil => factor
    case factor ~ repFactors =>
      repFactors.foldLeft(factor) {
        case (factorSoFar, "*" ~ nextFactor) => Prod(factorSoFar, nextFactor)
        case (factorSoFar, "/" ~ nextFactor) => Divide(factorSoFar, nextFactor)
      }
  }

  def factor: Parser[ParExpression] = expo ~ rep("^" ~ expo) ^^ {
    case expo ~ Nil => expo
    case expo ~ repExpos =>
      repExpos.foldLeft(expo) { case (base, "^" ~ exponent) =>
        Power(base, exponent)
      }
  }

  def expo: Parser[ParExpression] = exp | op

  def exp: Parser[ParExpression] = "e".r ^^ (_ => Expo(Constant(math.E)))

  def variable: Parser[ParExpression] = rep1(variableWithExp) ^^ { case varExps =>
    varExps.reduceLeft(Prod)
  }

  def variableWithExp: Parser[ParExpression] =
    """[a-zA-Z]""".r ~ ("^" ~> floatingPointNumber).? ^^ {
      case varName ~ None      => Var(varName)
      case varName ~ Some(exp) => Power(Var(varName), Constant(exp.toDouble))
    }

  def const: Parser[ParExpression] =
    floatingPointNumber ~ rep(variableWithExp) ^^ {
      case numStr ~ Nil => Constant(numStr.toDouble)
      case numStr ~ varExps =>
        varExps.foldLeft(Constant(numStr.toDouble): ParExpression)(Prod)
    }

  def sin: Parser[ParExpression] = ("sin(" ~> expr <~ ")") ^^ { case ex =>
    Sin(ex)
  }

  def cos: Parser[ParExpression] = ("cos(" ~> expr <~ ")") ^^ { case ex =>
    Cos(ex)
  }

  def tan: Parser[ParExpression] = ("tan(" ~> expr <~ ")") ^^ { case ex =>
    Tan(ex)
  }

  def sec: Parser[ParExpression] = ("sec(" ~> expr <~ ")") ^^ { case ex =>
    Sec(ex)
  }

  def csc: Parser[ParExpression] = ("csc(" ~> expr <~ ")") ^^ { case ex =>
    Csc(ex)
  }

  def cot: Parser[ParExpression] = ("cot(" ~> expr <~ ")") ^^ { case ex =>
    Cot(ex)
  }

  def arcsin: Parser[ParExpression] = ("arcsin(" ~> expr <~ ")") ^^ { case ex =>
    ArcSin(ex)
  }

  def arccos: Parser[ParExpression] = ("arccos(" ~> expr <~ ")") ^^ { case ex =>
    ArcCos(ex)
  }

  def arctan: Parser[ParExpression] = ("arctan(" ~> expr <~ ")") ^^ { case ex =>
    ArcTan(ex)
  }

  def arcsec: Parser[ParExpression] = ("arcsec(" ~> expr <~ ")") ^^ { case ex =>
    ArcSec(ex)
  }

  def arccsc: Parser[ParExpression] = ("arccsc(" ~> expr <~ ")") ^^ { case ex =>
    ArcCsc(ex)
  }

  def arccot: Parser[ParExpression] = ("arccot(" ~> expr <~ ")") ^^ { case ex =>
    ArcCot(ex)
  }

  def ln: Parser[ParExpression] = ("ln(" ~> expr <~ ")") ^^ { case ex =>
    Ln(ex)
  }

  def apply(input: String): Option[ParExpression] =
    parseAll(expr, input) match {
      case Success(result, _) => Some(result)
      case _                  => None
    }
}
