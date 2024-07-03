package solver

import com.sun.org.apache.xpath.internal.operations.Variable

sealed trait Expression {
  var value: Double = 0
  var partial: Double = 0

  def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial

  def derive(seed: Double): Unit
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

  override def derive(seed: Double): Unit = {
    partial += 0
  }
}

case class Var(name: String) extends Expression {
  override def toString = s"Var($name)"

  override def evaluateAndDerive(
                                  varAssn: Map[String, Double], variable: String
                                ): ValueAndPartial = {
    value = varAssn(name)
    partial = if (name == variable) 1 else 0
    ValueAndPartial(value, partial)
  }


  override def derive(seed: Double): Unit = {
    partial += seed
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

  override def derive(seed: Double): Unit = {
    e1.derive(seed)
    e2.derive(seed)
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

  override def derive(seed: Double): Unit = {
    e1.derive(e2.value * seed)
    e2.derive(e1.value * seed)
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

  override def derive(seed: Double): Unit = {
    b.derive(e.value * Math.pow(b.value, e.value - 1) * seed)
    e.derive(Math.pow(b.value, e.value) * Math.log(b.value) * seed)
  }
}
