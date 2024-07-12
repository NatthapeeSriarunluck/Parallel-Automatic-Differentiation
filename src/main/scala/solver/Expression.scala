package solver

sealed trait Expression {
  var value: Double = 0
  var partial: Double = 0

  def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial
  def derive(seed: Double, varAssn: Map[String, Double]): Unit
  def findVar(name: String): Option[Var] = this match {
      case v: Var if v.name == name => Some(v)
      case Sum(e1, e2) => e1.findVar(name).orElse(e2.findVar(name))
      case Prod(e1, e2) => e1.findVar(name).orElse(e2.findVar(name))
      case Power(b, e) => b.findVar(name).orElse(e.findVar(name))
      case Sin(e) => e.findVar(name)
      case Cos(e) => e.findVar(name)
      case Tan(e) => e.findVar(name)
      case _ => None
    }
}
object PartialDerivativeOf  {
  var grads: scala.collection.mutable.Map[String, Double] = scala.collection.mutable.Map()
}

case class ValueAndPartial(value: Double, partial: Double) {
  def toList: List[Double] = List(value, partial)
}

case class Constant(n: Double) extends Expression {
  override def toString: String = s"Const($n)"

  override def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    ValueAndPartial(n, 0)
  }
  override def derive(seed: Double, varAssn: Map[String, Double]): Unit = {
  }
}

case class Var(name: String) extends Expression {
  var grad: Double = 0
  override def toString: String = s"Var($name)"

  override def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    value = varAssn(name)
    partial = if (name == variable) 1 else 0
    ValueAndPartial(value, partial)
  }


  override def derive(seed: Double, varAssn: Map[String, Double]): Unit = {
    PartialDerivativeOf.grads(name) = PartialDerivativeOf.grads.getOrElse(name, 0.0) + seed
  }
}

case class Sum(e1: Expression, e2: Expression) extends Expression {
  override def toString: String = s"Sum(${e1.toString}, ${e2.toString})"

  override def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    val vp1 = e1.evaluateAndDerive(varAssn, variable)
    val vp2 = e2.evaluateAndDerive(varAssn, variable)
    ValueAndPartial(vp1.value + vp2.value, vp1.partial + vp2.partial)
  }

  override def derive(seed: Double, varAssn: Map[String, Double]): Unit = {
    e1.value = Process.eval(e1, varAssn)
    e2.value = Process.eval(e2, varAssn)
    e1.derive(seed, varAssn)
    e2.derive(seed, varAssn)
  }
}

case class Prod(e1: Expression, e2: Expression) extends Expression {
  override def toString: String = s"Prod(${e1.toString}, ${e2.toString})"

  override def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    val vp1 = e1.evaluateAndDerive(varAssn, variable)
    val vp2 = e2.evaluateAndDerive(varAssn, variable)
    ValueAndPartial(vp1.value * vp2.value, vp2.value * vp1.partial + vp1.value * vp2.partial)
  }

  override def derive(seed: Double, varAssn: Map[String, Double] ): Unit = {
    e1.value = Process.eval(e1, varAssn)
    e2.value = Process.eval(e2, varAssn)
    e1.derive(seed * e2.value, varAssn)
    e2.derive(seed * e1.value, varAssn)
  }
}

case class Power(b: Expression, e: Expression) extends Expression {
  override def toString: String = s"Power(${b.toString}, ${e.toString})"

  override def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    val vpB = b.evaluateAndDerive(varAssn, variable)
    val vpE = e.evaluateAndDerive(varAssn, variable)
    val value = Math.pow(vpB.value, vpE.value)
    val partial = value * (vpE.value / vpB.value * vpB.partial + Math.log(vpB.value) * vpE.partial)
    ValueAndPartial(value, partial)
  }

  override def derive(seed: Double, varAssn: Map[String, Double] ): Unit = {
    b.value = Process.eval(b, varAssn)
    e.value = Process.eval(e, varAssn)
    b.derive(seed * e.value * Math.pow(b.value, e.value - 1), varAssn)
    e.derive(seed * Math.pow(b.value, e.value) * Math.log(b.value), varAssn)
  }
}

case class Sin(e: Expression) extends Expression {
  override def toString: String = s"Sin(${e.toString})"

  override def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    val vp = e.evaluateAndDerive(varAssn, variable)
    val value = Math.sin(vp.value)
    val partial = Math.cos(vp.value) * vp.partial
    ValueAndPartial(value, partial)
  }

  override def derive(seed: Double, varAssn: Map[String, Double] ): Unit = {
    e.value = Process.eval(e, varAssn)
    e.derive(seed * Math.cos(e.value), varAssn)
  }
}

case class Cos(e: Expression) extends Expression {
  override def toString: String = s"Cos(${e.toString})"

  override def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    val vp = e.evaluateAndDerive(varAssn, variable)
    val value = Math.cos(vp.value)
    val partial = -Math.sin(vp.value) * vp.partial
    ValueAndPartial(value, partial)
  }

  override def derive(seed: Double, varAssn: Map[String, Double] ): Unit = {
    e.value = Process.eval(e, varAssn)
    e.derive(seed * -Math.sin(e.value), varAssn)
  }
}

case class Tan(e: Expression) extends Expression {
  override def toString: String = s"Tan(${e.toString})"

  override def evaluateAndDerive(varAssn: Map[String, Double], variable: String): ValueAndPartial = {
    val vp = e.evaluateAndDerive(varAssn, variable)
    val value = Math.tan(vp.value)
    val partial = vp.partial / Math.pow(Math.cos(vp.value), 2)
    ValueAndPartial(value, partial)
  }

  override def derive(seed: Double, varAssn: Map[String, Double] ): Unit = {
    e.value = Process.eval(e, varAssn)
    e.derive(seed / Math.pow(Math.cos(e.value), 2), varAssn)
  }
}