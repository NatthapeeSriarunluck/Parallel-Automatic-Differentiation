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
  def expr: Parser[Par_Expression] = term ~ (("+" | "-") ~ term).* ^^ {
    case term ~ Nil => term
    case term ~ repTerms =>
      repTerms.foldLeft(term) {
        case (termSoFar, "+" ~ nextTerm) => Sum(termSoFar, nextTerm)
        case (termSoFar, "-" ~ nextTerm) =>
          Sum(termSoFar, Prod(Constant(-1), nextTerm))
        case _ => throw new Exception("shouldn't get here")
      }
  }
  def term: Parser[Par_Expression] = expo ~ ("*" ~ expo).* ^^ {
    case ex ~ Nil => ex
    case ex ~ repExs =>
      repExs.foldLeft(ex) {
        case (exSoFar, "*" ~ nextEx) => Prod(exSoFar, nextEx)
        case _                       => throw new Exception("shouldn't get here")
      }
  }

  def expo: Parser[Par_Expression] = op ~ ("^" ~ op).? ^^ {
    case op ~ None               => op
    case base ~ Some("^" ~ expn) => Power(base, expn)
    case _                       => throw new Exception("shouldn't get here")
  }

  def variable: Parser[Par_Expression] = rep1("""[a-zA-Z]""".r) ^^ {
    case varNames if varNames.length == 1 => Var(varNames.head)
    case varNames                         => varNames.map(Var).reduceLeft(Prod)
  }

  def variableWithExpo: Parser[Par_Expression] =
    rep1("""[a-zA-Z]""".r) ~ "^" ~ floatingPointNumber ^^ {
      case varNames ~ "^" ~ expo =>
        Power(Var(varNames.head), Constant(expo.toFloat))
    }

  def const: Parser[Par_Expression] = floatingPointNumber ~ opt(variable) ^^ {
    case numStr ~ None           => Constant(numStr.toFloat)
    case numStr ~ Some(variable) => Prod(Constant(numStr.toFloat), variable)
  }

  def exp: Parser[Par_Expression] = "e".r ^^ (_ => Expo(Constant(math.E)))

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

  def arcsin: Parser[Par_Expression] = ("arcsin(" ~> expr <~ ")") ^^ {
    case ex =>
      ArcSin(ex)
  }

  def arccos: Parser[Par_Expression] = ("arccos(" ~> expr <~ ")") ^^ {
    case ex =>
      ArcCos(ex)
  }

  def arctan: Parser[Par_Expression] = ("arctan(" ~> expr <~ ")") ^^ {
    case ex =>
      ArcTan(ex)
  }

  def arcsec: Parser[Par_Expression] = ("arcsec(" ~> expr <~ ")") ^^ {
    case ex =>
      ArcSec(ex)
  }

  def arccsc: Parser[Par_Expression] = ("arccsc(" ~> expr <~ ")") ^^ {
    case ex =>
      ArcCsc(ex)
  }

  def arccot: Parser[Par_Expression] = ("arccot(" ~> expr <~ ")") ^^ {
    case ex =>
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
