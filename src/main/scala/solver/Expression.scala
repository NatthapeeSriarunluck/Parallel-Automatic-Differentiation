package solver

import com.sun.org.apache.xpath.internal.operations.Variable

sealed trait Expression {
  def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial
}

case class ValueAndPartial(value: Double, partial: Double) {
  def toList: List[Double] = List(value, partial)
}

case class Constant(n: Double) extends Expression {
  override def toString = s"Const($n)"

  override def evaluateAndDerive(
      varAssn: Map[String, Double],
      variable: String
  ): ValueAndPartial = {
    ValueAndPartial(n, 0)
  }
}

case class Var(name: String) extends Expression {
  override def toString = s"Var($name)"

  override def evaluateAndDerive(
      varAssn: Map[String, Double], variable: String
  ): ValueAndPartial = {
    val value = varAssn(name)
    val partial = if (name == variable) 1 else 0
    ValueAndPartial(value, partial)
  }
}

case class Sum(e1: Expression, e2: Expression) extends Expression {
  override def toString = {
    val (l, r) = (e1.toString, e2.toString)
    s"Sum($l, $r)"
  }

  override def evaluateAndDerive(
      varAssn: Map[String, Double],
      variable: String
  ): ValueAndPartial = {
    val ValueAndPartial(valueA, partialA) = e1.evaluateAndDerive(varAssn, variable)
    val ValueAndPartial(valueB, partialB) = e2.evaluateAndDerive(varAssn, variable)
    ValueAndPartial(valueA + valueB, partialA + partialB)

  }
}

case class Prod(e1: Expression, e2: Expression) extends Expression {
  override def toString = {
    val (l, r) = (e1.toString, e2.toString)

    s"Prod($l, $r)"
  }
  override def evaluateAndDerive(
      varAssn: Map[String, Double],
      variable: String
  ): ValueAndPartial = {
    val ValueAndPartial(valueA, partialA) = e1.evaluateAndDerive(varAssn, variable)
    val ValueAndPartial(valueB, partialB) = e2.evaluateAndDerive(varAssn, variable)
    ValueAndPartial(valueA * valueB, valueB * partialA + valueA * partialB)
  }
}

case class Power(b: Expression, e: Expression) extends Expression {
  override def toString = {
    val (bb, ee) = (b.toString, e.toString)
    s"Power($bb, $ee)"
  }
  override def evaluateAndDerive(
      varAssn: Map[String, Double],
      variable: String
  ): ValueAndPartial = {
    val ValueAndPartial(valueB, partialB) = b.evaluateAndDerive(varAssn, variable)
    val ValueAndPartial(valueE, partialE) = e.evaluateAndDerive(varAssn, variable)
    val value = Math.pow(valueB, valueE)
    val partial = value * (valueE / valueB * partialB + Math.log(valueB) * partialE)
    ValueAndPartial(value, partial)
  }
}
