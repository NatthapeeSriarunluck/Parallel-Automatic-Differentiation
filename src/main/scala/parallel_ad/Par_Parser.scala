package parallel_ad

import scala.util.parsing.combinator.*

/*
 * The parser object which produces a parsed expression for a given string.
 * As an example,
 *    val e = solver.Parser("x^2 + 3*x - 1")
 */

object Par_Parser extends JavaTokenParsers {

  def op: Parser[Par_Expression] = (sin | cos | tan | ln | const | variable |  ("(" ~> expr <~ ")") )
  def expr: Parser[Par_Expression] = term ~ (("+"|"-") ~ term).*  ^^ {
    case term ~ Nil => term
    case term ~ repTerms => repTerms.foldLeft(term) {
      case (termSoFar, "+" ~ nextTerm) => Sum(termSoFar, nextTerm)
      case (termSoFar, "-" ~ nextTerm) => Sum(termSoFar, Prod(Constant(-1), nextTerm))
      case _ => throw new Exception("shouldn't get here")
    }
  }
  def term: Parser[Par_Expression] = expo ~ ("*" ~ expo).*  ^^ {
    case ex ~ Nil => ex
    case ex ~ repExs => repExs.foldLeft(ex) {
      case (exSoFar, "*" ~ nextEx) => Prod(exSoFar, nextEx)
      case _ => throw new Exception("shouldn't get here")
    }
  }

  def expo: Parser[Par_Expression] = op ~ ("^" ~ op).?  ^^ {
    case op ~ None => op
    case base ~ Some("^" ~ expn) => Power(base, expn)
    case _ => throw new Exception("shouldn't get here")
  }

  def variable: Parser[Par_Expression] = rep1("""[a-zA-Z]""".r) ^^ {
    case varNames if varNames.length == 1 => Var(varNames.head)
    case varNames => varNames.map(Var).reduceLeft(Prod)
  }


  def const: Parser[Par_Expression] = floatingPointNumber ~ opt(variable) ^^ {
    case numStr ~ None => Constant(numStr.toFloat)
    case numStr ~ Some(variable) => Prod(Constant(numStr.toFloat), variable)
  }
  
  def sin: Parser[Par_Expression] = ("sin(" ~> expr <~ ")") ^^ {
    case ex => Sin(ex)
  }

  def cos: Parser[Par_Expression] = ("cos(" ~> expr <~ ")") ^^ {
    case ex => Cos(ex)
  }

  def tan: Parser[Par_Expression] = ("tan(" ~> expr <~ ")") ^^ {
    case ex => Tan(ex)
  }

  def ln: Parser[Par_Expression] = ("ln(" ~> expr <~ ")") ^^ {
    case ex => Ln(ex)
  }

  def apply(input: String): Option[Par_Expression] =
    parseAll(expr, input) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
}