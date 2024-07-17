
package sequential_ad

import scala.util.parsing.combinator.*

/*
 * The parser object which produces a parsed expression for a given string.
 * As an example,
 *    val e = solver.Parser("x^2 + 3*x - 1")
 */

object Parser extends JavaTokenParsers {

  def op: Parser[Expression] =
    (sin | cos | tan | sec | csc | cot | arcsin | arccos | arctan | arcsec | arccsc | arccot | ln | exp | const | variable | ("(" ~> expr <~ ")"))

  def expr: Parser[Expression] = term ~ rep(("+" | "-") ~ term) ^^ {
    case term ~ Nil => term
    case term ~ repTerms =>
      repTerms.foldLeft(term) {
        case (termSoFar, "+" ~ nextTerm) => Sum(termSoFar, nextTerm)
        case (termSoFar, "-" ~ nextTerm) =>
          Sum(termSoFar, Prod(Constant(-1), nextTerm))
      }
  }

  def term: Parser[Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case factor ~ Nil => factor
    case factor ~ repFactors =>
      repFactors.foldLeft(factor) {
        case (factorSoFar, "*" ~ nextFactor) => Prod(factorSoFar, nextFactor)
        case (factorSoFar, "/" ~ nextFactor) => Divide(factorSoFar, nextFactor)
      }
  }

  def factor: Parser[Expression] = expo ~ rep("^" ~ expo) ^^ {
    case expo ~ Nil => expo
    case expo ~ repExpos =>
      repExpos.foldLeft(expo) {
        case (base, "^" ~ exponent) => Power(base, exponent)
      }
  }

  def expo: Parser[Expression] = exp | op

  def exp: Parser[Expression] = "e".r ^^ (_ => Expo(Constant(math.E)))

  def variable: Parser[Expression] = rep1(variableWithExp) ^^ { case varExps =>
    varExps.reduceLeft(Prod)
  }

  def variableWithExp: Parser[Expression] =
    """[a-zA-Z]""".r ~ ("^" ~> floatingPointNumber).? ^^ {
      case varName ~ None      => Var(varName)
      case varName ~ Some(exp) => Power(Var(varName), Constant(exp.toDouble))
    }

  def const: Parser[Expression] =
    floatingPointNumber ~ rep(variableWithExp) ^^ {
      case numStr ~ Nil => Constant(numStr.toDouble)
      case numStr ~ varExps =>
        varExps.foldLeft(Constant(numStr.toDouble): Expression)(Prod)
    }

  def sin: Parser[Expression] = ("sin(" ~> expr <~ ")") ^^ { case ex =>
    Sin(ex)
  }

  def cos: Parser[Expression] = ("cos(" ~> expr <~ ")") ^^ { case ex =>
    Cos(ex)
  }

  def tan: Parser[Expression] = ("tan(" ~> expr <~ ")") ^^ { case ex =>
    Tan(ex)
  }

  def sec: Parser[Expression] = ("sec(" ~> expr <~ ")") ^^ { case ex =>
    Sec(ex)
  }

  def csc: Parser[Expression] = ("csc(" ~> expr <~ ")") ^^ { case ex =>
    Csc(ex)
  }

  def cot: Parser[Expression] = ("cot(" ~> expr <~ ")") ^^ { case ex =>
    Cot(ex)
  }

  def arcsin: Parser[Expression] = ("arcsin(" ~> expr <~ ")") ^^ { case ex =>
    ArcSin(ex)
  }

  def arccos: Parser[Expression] = ("arccos(" ~> expr <~ ")") ^^ { case ex =>
    ArcCos(ex)
  }

  def arctan: Parser[Expression] = ("arctan(" ~> expr <~ ")") ^^ { case ex =>
    ArcTan(ex)
  }

  def arcsec: Parser[Expression] = ("arcsec(" ~> expr <~ ")") ^^ { case ex =>
    ArcSec(ex)
  }

  def arccsc: Parser[Expression] = ("arccsc(" ~> expr <~ ")") ^^ { case ex =>
    ArcCsc(ex)
  }

  def arccot: Parser[Expression] = ("arccot(" ~> expr <~ ")") ^^ { case ex =>
    ArcCot(ex)
  }

  def ln: Parser[Expression] = ("ln(" ~> expr <~ ")") ^^ { case ex =>
    Ln(ex)
  }

  def apply(input: String): Option[Expression] =
    parseAll(expr, input) match {
      case Success(result, _) => Some(result)
      case _                  => None
    }
}
