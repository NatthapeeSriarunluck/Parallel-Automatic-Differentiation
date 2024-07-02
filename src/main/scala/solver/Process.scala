object Process {
  // gives a "pretty-print" string form of the expression
  def stringify(e: Expression): String = e match {
    case Constant(c) => c.toString
    case Var(name)   => name
    case Sum(l, r)   => stringify(l) + " + " + stringify(r)
    case Prod(l @ Sum(_, _), r @ Sum(_, _)) =>
      "(" + stringify(l) + ") * (" + stringify(r) + ")"
    case Prod(l @ Sum(_, _), r) => "(" + stringify(l) + ") * " + stringify(r)
    case Prod(l, r @ Sum(_, _)) => stringify(l) + " * (" + stringify(r) + ")"
    case Prod(l, r)             => stringify(l) + " * " + stringify(r)
    case Power(b, e)            => stringify(b) + "^" + stringify(e)
  }

  // evaluates a given expression e: Expression using
  // the variable settings in varAssn: Map[String, Double],
  // returning the evaluation result as a Double.

  // Example: eval(e, Map("x" -> 4.0)) evaluates the expression
  // with the variable "x" set to 4.0.
  def eval(e: Expression, varAssn: Map[String, Double]): Double = e match {
    case Constant(n)   => n
    case Var(name)     => varAssn(name)
    case Sum(e1, e2)   => eval(e1, varAssn) + eval(e2, varAssn)
    case Prod(e1, e2)  => eval(e1, varAssn) * eval(e2, varAssn)
    case Power(e1, e2) => Math.pow(eval(e1, varAssn), eval(e2, varAssn))
  }
  // The method call expects the expression Z to be derived with regard to a variable V. The method returns a pair of the evaluated function and its derivation.
  def evaluateAndDerive(
      Z: Expression,
      V: String
  ): (Double, Double) = {}

  // forms a new expression that simplifies the given expression e: Expression
  // the goal of this function is produce an expression that is easier to
  // evaluate and/or differentiate.  If there's a canonical form you'd like to
  // follow, use this function to put an expression in this form.
  // you may leave this function blank if can't find any use.
  //
  def simplify(e: Expression): Expression = ???

}