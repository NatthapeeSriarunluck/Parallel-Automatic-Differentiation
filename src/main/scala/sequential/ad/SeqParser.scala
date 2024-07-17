
package sequential.ad

import scala.util.parsing.combinator.*

/*
 * The parser object which produces a parsed expression for a given string.
 * As an example,
 *    val e = solver.Parser("x^2 + 3*x - 1")
 */

object SeqParser extends JavaTokenParsers {

  def op: Parser[SeqExpression] =
    (sin | cos | tan | sec | csc | cot | arcsin | arccos | arctan | arcsec | arccsc | arccot | ln | exp | const | variable | ("(" ~> expr <~ ")"))

  def expr: Parser[SeqExpression] = term ~ rep(("+" | "-") ~ term) ^^ {
    case term ~ Nil => term
    case term ~ repTerms =>
      repTerms.foldLeft(term) {
        case (termSoFar, "+" ~ nextTerm) => Sum(termSoFar, nextTerm)
        case (termSoFar, "-" ~ nextTerm) =>
          Sum(termSoFar, Prod(Constant(-1), nextTerm))
      }
  }

  def term: Parser[SeqExpression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case factor ~ Nil => factor
    case factor ~ repFactors =>
      repFactors.foldLeft(factor) {
        case (factorSoFar, "*" ~ nextFactor) => Prod(factorSoFar, nextFactor)
        case (factorSoFar, "/" ~ nextFactor) => Divide(factorSoFar, nextFactor)
      }
  }

  def factor: Parser[SeqExpression] = expo ~ rep("^" ~ expo) ^^ {
    case expo ~ Nil => expo
    case expo ~ repExpos =>
      repExpos.foldLeft(expo) {
        case (base, "^" ~ exponent) => Power(base, exponent)
      }
  }

  def expo: Parser[SeqExpression] = exp | op

  def exp: Parser[SeqExpression] = "e".r ^^ (_ => Expo(Constant(math.E)))

  def variable: Parser[SeqExpression] = rep1(variableWithExp) ^^ { case varExps =>
    varExps.reduceLeft(Prod)
  }

  def variableWithExp: Parser[SeqExpression] =
    """[a-zA-Z]""".r ~ ("^" ~> floatingPointNumber).? ^^ {
      case varName ~ None      => Var(varName)
      case varName ~ Some(exp) => Power(Var(varName), Constant(exp.toDouble))
    }

  def const: Parser[SeqExpression] =
    floatingPointNumber ~ rep(variableWithExp) ^^ {
      case numStr ~ Nil => Constant(numStr.toDouble)
      case numStr ~ varExps =>
        varExps.foldLeft(Constant(numStr.toDouble): SeqExpression)(Prod)
    }

  def sin: Parser[SeqExpression] = ("sin(" ~> expr <~ ")") ^^ { case ex =>
    Sin(ex)
  }

  def cos: Parser[SeqExpression] = ("cos(" ~> expr <~ ")") ^^ { case ex =>
    Cos(ex)
  }

  def tan: Parser[SeqExpression] = ("tan(" ~> expr <~ ")") ^^ { case ex =>
    Tan(ex)
  }

  def sec: Parser[SeqExpression] = ("sec(" ~> expr <~ ")") ^^ { case ex =>
    Sec(ex)
  }

  def csc: Parser[SeqExpression] = ("csc(" ~> expr <~ ")") ^^ { case ex =>
    Csc(ex)
  }

  def cot: Parser[SeqExpression] = ("cot(" ~> expr <~ ")") ^^ { case ex =>
    Cot(ex)
  }

  def arcsin: Parser[SeqExpression] = ("arcsin(" ~> expr <~ ")") ^^ { case ex =>
    ArcSin(ex)
  }

  def arccos: Parser[SeqExpression] = ("arccos(" ~> expr <~ ")") ^^ { case ex =>
    ArcCos(ex)
  }

  def arctan: Parser[SeqExpression] = ("arctan(" ~> expr <~ ")") ^^ { case ex =>
    ArcTan(ex)
  }

  def arcsec: Parser[SeqExpression] = ("arcsec(" ~> expr <~ ")") ^^ { case ex =>
    ArcSec(ex)
  }

  def arccsc: Parser[SeqExpression] = ("arccsc(" ~> expr <~ ")") ^^ { case ex =>
    ArcCsc(ex)
  }

  def arccot: Parser[SeqExpression] = ("arccot(" ~> expr <~ ")") ^^ { case ex =>
    ArcCot(ex)
  }

  def ln: Parser[SeqExpression] = ("ln(" ~> expr <~ ")") ^^ { case ex =>
    Ln(ex)
  }

  def apply(input: String): Option[SeqExpression] =
    parseAll(expr, input) match {
      case Success(result, _) => Some(result)
      case _                  => None
    }
}
