package parallel_ad

object Par_Process {
  // gives a "pretty-print" string form of the expression
  def stringify(e: Par_Expression): String = e match {
    case Constant(c) => c.toString
    case Var(name)   => name
    case Sum(l, r)   => stringify(l) + " + " + stringify(r)
    case Prod(l @ Sum(_, _), r @ Sum(_, _)) =>
      "(" + stringify(l) + ") * (" + stringify(r) + ")"
    case Prod(l @ Sum(_, _), r) => "(" + stringify(l) + ") * " + stringify(r)
    case Prod(l, r @ Sum(_, _)) => stringify(l) + " * (" + stringify(r) + ")"
    case Prod(l, r)             => stringify(l) + " * " + stringify(r)
    case Power(b, e)            => stringify(b) + "^" + stringify(e)
    case Sin(e)                 => "sin(" + stringify(e) + ")"
    case Cos(e)                 => "cos(" + stringify(e) + ")"
    case Tan(e)                 => "tan(" + stringify(e) + ")"
    case Sec(e)                => "sec(" + stringify(e) + ")"
    case Csc(e)                => "csc(" + stringify(e) + ")"
    case Cot(e)                => "cot(" + stringify(e) + ")"
    case ArcSin(e)             => "arcsin(" + stringify(e) + ")"
    case ArcCos(e)             => "arccos(" + stringify(e) + ")"
    case ArcTan(e)             => "arctan(" + stringify(e) + ")"
    case ArcSec(e)             => "arcsec(" + stringify(e) + ")"
    case ArcCsc(e)             => "arccsc(" + stringify(e) + ")"
    case ArcCot(e)             => "arccot(" + stringify(e) + ")"
    case Ln(e)                  => "ln(" + stringify(e) + ")"
  }

  // evaluates a given expression e: Par_Expression using
  // the variable settings in varAssn: Map[String, Double],
  // returning the evaluation result as a Double.

  // Example: eval(e, Map("x" -> 4.0)) evaluates the expression
  // with the variable "x" set to 4.0.
  def eval(e: Par_Expression, varAssn: Map[String, Double]): Double = e match {
    case Constant(n)   => n
    case Var(name)     => varAssn(name)
    case Sum(e1, e2)   => eval(e1, varAssn) + eval(e2, varAssn)
    case Prod(e1, e2)  => eval(e1, varAssn) * eval(e2, varAssn)
    case Power(e1, e2) => Math.pow(eval(e1, varAssn), eval(e2, varAssn))
    case Sin(e)        => Math.sin(eval(e, varAssn))
    case Cos(e)        => Math.cos(eval(e, varAssn))
    case Tan(e)        => Math.tan(eval(e, varAssn))
    case Sec(e)        => 1 / Math.cos(eval(e, varAssn))
    case Csc(e)        => 1 / Math.sin(eval(e, varAssn))
    case Cot(e)        => 1 / Math.tan(eval(e, varAssn))
    case ArcSin(e)     => Math.asin(eval(e, varAssn))
    case ArcCos(e)     => Math.acos(eval(e, varAssn))
    case ArcTan(e)     => Math.atan(eval(e, varAssn))
    case ArcSec(e)     => Math.acos(1 / eval(e, varAssn))
    case ArcCsc(e)     => Math.asin(1 / eval(e, varAssn))
    case ArcCot(e)     => Math.atan(1 / eval(e, varAssn))

    case Ln(e)         => Math.log(eval(e, varAssn))
  }

  // forms a new expression that simplifies the given expression e: Par_Expression
  // the goal of this function is produce an expression that is easier to
  // evaluate and/or differentiate.  If there's a canonical form you'd like to
  // follow, use this function to put an expression in this form.
  // you may leave this function blank if can't find any use.
  //
  def simplify(e: Par_Expression): Par_Expression = ???

}
