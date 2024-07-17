package parallel_ad

import scala.util.parsing.combinator.*

/*
 * The parser object which produces a parsed expression for a given string.
 * As an example,
 *    val e = solver.Parser("x^2 + 3*x - 1")
 */

object Par_Parser extends JavaTokenParsers {

  def op: Parser[Par_Expression] =
    (sin | cos | tan | sec | csc | cot | arcsin | arccos | arctan | arcsec | arccsc | arccot | ln | exp | const | variable | ("(" ~> expr <~ ")"))

  def expr: Parser[Par_Expression] = term ~ rep(("+" | "-") ~ term) ^^ {
    case term ~ Nil => term
    case term ~ repTerms =>
      repTerms.foldLeft(term) {
        case (termSoFar, "+" ~ nextTerm) => Sum(termSoFar, nextTerm)
        case (termSoFar, "-" ~ nextTerm) =>
          Sum(termSoFar, Prod(Constant(-1), nextTerm))
      }
  }

  def term: Parser[Par_Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case factor ~ Nil => factor
    case factor ~ repFactors =>
      repFactors.foldLeft(factor) {
        case (factorSoFar, "*" ~ nextFactor) => Prod(factorSoFar, nextFactor)
        case (factorSoFar, "/" ~ nextFactor) => Divide(factorSoFar, nextFactor)
      }
  }

  def factor: Parser[Par_Expression] = expo ~ rep("^" ~ expo) ^^ {
    case expo ~ Nil => expo
    case expo ~ repExpos =>
      repExpos.foldLeft(expo) {
        case (base, "^" ~ exponent) => Power(base, exponent)
      }
  }

  def expo: Parser[Par_Expression] = exp | op

  def exp: Parser[Par_Expression] = "e".r ^^ (_ => Expo(Constant(math.E)))

  def variable: Parser[Par_Expression] = rep1(variableWithExp) ^^ { case varExps =>
    varExps.reduceLeft(Prod)
  }

  def variableWithExp: Parser[Par_Expression] =
    """[a-zA-Z]""".r ~ ("^" ~> floatingPointNumber).? ^^ {
      case varName ~ None      => Var(varName)
      case varName ~ Some(exp) => Power(Var(varName), Constant(exp.toDouble))
    }

  def const: Parser[Par_Expression] =
    floatingPointNumber ~ rep(variableWithExp) ^^ {
      case numStr ~ Nil => Constant(numStr.toDouble)
      case numStr ~ varExps =>
        varExps.foldLeft(Constant(numStr.toDouble): Par_Expression)(Prod)
    }

  def sin: Parser[Par_Expression] = ("sin(" ~> expr <~ ")") ^^ { case ex =>
    Sin(ex)
  }

  def cos: Parser[Par_Expression] = ("cos(" ~> expr <~ ")") ^^ { case ex =>
    Cos(ex)
  }

  def tan: Parser[Par_Expression] = ("tan(" ~> expr <~ ")") ^^ { case ex =>
    Tan(ex)
  }

  def sec: Parser[Par_Expression] = ("sec(" ~> expr <~ ")") ^^ { case ex =>
    Sec(ex)
  }

  def csc: Parser[Par_Expression] = ("csc(" ~> expr <~ ")") ^^ { case ex =>
    Csc(ex)
  }

  def cot: Parser[Par_Expression] = ("cot(" ~> expr <~ ")") ^^ { case ex =>
    Cot(ex)
  }

  def arcsin: Parser[Par_Expression] = ("arcsin(" ~> expr <~ ")") ^^ { case ex =>
    ArcSin(ex)
  }

  def arccos: Parser[Par_Expression] = ("arccos(" ~> expr <~ ")") ^^ { case ex =>
    ArcCos(ex)
  }

  def arctan: Parser[Par_Expression] = ("arctan(" ~> expr <~ ")") ^^ { case ex =>
    ArcTan(ex)
  }

  def arcsec: Parser[Par_Expression] = ("arcsec(" ~> expr <~ ")") ^^ { case ex =>
    ArcSec(ex)
  }

  def arccsc: Parser[Par_Expression] = ("arccsc(" ~> expr <~ ")") ^^ { case ex =>
    ArcCsc(ex)
  }

  def arccot: Parser[Par_Expression] = ("arccot(" ~> expr <~ ")") ^^ { case ex =>
    ArcCot(ex)
  }

  def ln: Parser[Par_Expression] = ("ln(" ~> expr <~ ")") ^^ { case ex =>
    Ln(ex)
  }

  def apply(input: String): Option[Par_Expression] =
    parseAll(expr, input) match {
      case Success(result, _) => Some(result)
      case _                  => None
    }
}