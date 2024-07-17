package sequential_ad

sealed trait Expression {
  def forward(varAssn: Map[String, Double], variable: String): ValueAndPartial
  def reverse(seed: Double, varAssn: Map[String, Double]): Unit

  def findVar(name: String): Option[Var] = this match {
    case v: Var if v.name == name => Some(v)
    case Sum(e1, e2)              => e1.findVar(name).orElse(e2.findVar(name))
    case Prod(e1, e2)             => e1.findVar(name).orElse(e2.findVar(name))
    case Divide(e1, e2)           => e1.findVar(name).orElse(e2.findVar(name))
    case Power(b, e)              => b.findVar(name).orElse(e.findVar(name))
    case Sin(e)                   => e.findVar(name)
    case Cos(e)                   => e.findVar(name)
    case Tan(e)                   => e.findVar(name)
    case Ln(e)                    => e.findVar(name)
    case _                        => None
  }
}

object PartialDerivativeOf {

  var grads: scala.collection.mutable.Map[String, Double] =
    scala.collection.mutable.Map()

  def UpdateGrad(name: String, value: Double): Unit = {
    grads(name) = grads.getOrElse(name, 0.0) + value
  }
}

object ValueOf {

  var values: scala.collection.mutable.Map[String, Double] =
    scala.collection.mutable.Map()

  def UpdateValue(name: String, value: Double): Unit = {
    values(name) = value
  }
}

case class ValueAndPartial(value: Double, partial: Double) {
  def toList: List[Double] = List(value, partial)
}

case class Constant(n: Double) extends Expression {
  override def toString: String = s"Constant($n)"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    ValueAndPartial(n, 0)
  }
  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {}
}

case class Var(name: String) extends Expression {
  var grad: Double = 0
  override def toString: String = s"Var($name)"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val value = varAssn(name)
    ValueAndPartial(value, if (name == variable) 1 else 0)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    PartialDerivativeOf.grads(name) = PartialDerivativeOf.grads.getOrElse(name, 0.0) + seed
  }
}

case class Expo(e: Expression) extends Expression {
  override def toString: String = s"Expo(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    e.forward(varAssn, variable)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    e.reverse(seed, varAssn)
  }
}

case class Sum(e1: Expression, e2: Expression) extends Expression {
  override def toString: String = s"Sum(${e1.toString},${e2.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp1 = e1.forward(varAssn, variable)
    val vp2 = e2.forward(varAssn, variable)
    ValueAndPartial(vp1.value + vp2.value, vp1.partial + vp2.partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val e1Value = Process.eval(e1, varAssn)
    val e2Value = Process.eval(e2, varAssn)
    ValueOf.UpdateValue(e1.toString, e1Value)
    ValueOf.UpdateValue(e2.toString, e2Value)
    e1.reverse(seed, varAssn)
    e2.reverse(seed, varAssn)
  }
}

case class Prod(e1: Expression, e2: Expression) extends Expression {
  override def toString: String = s"Prod(${e1.toString},${e2.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp1 = e1.forward(varAssn, variable)
    val vp2 = e2.forward(varAssn, variable)
    ValueAndPartial(
      vp1.value * vp2.value,
      vp2.value * vp1.partial + vp1.value * vp2.partial
    )
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val e1Value = Process.eval(e1, varAssn)
    ValueOf.UpdateValue(e1.toString, e1Value)
    val e2Value = Process.eval(e2, varAssn)
    ValueOf.UpdateValue(e2.toString, e2Value)
    e1.reverse(seed * e2Value, varAssn)
    e2.reverse(seed * e1Value, varAssn)
  }
}

case class Divide(e1: Expression, e2: Expression) extends Expression {
  override def toString: String = s"Divide(${e1.toString},${e2.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp1 = e1.forward(varAssn, variable)
    val vp2 = e2.forward(varAssn, variable)
    ValueAndPartial(
      vp1.value / vp2.value,
      (vp1.partial * vp2.value - vp1.value * vp2.partial) / Math.pow(vp2.value, 2)
    )
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val e1Value = Process.eval(e1, varAssn)
    ValueOf.UpdateValue(e1.toString, e1Value)
    val e2Value = Process.eval(e2, varAssn)
    ValueOf.UpdateValue(e2.toString, e2Value)
    e1.reverse(seed / e2Value, varAssn)
    e2.reverse(-seed * e1Value / Math.pow(e2Value, 2), varAssn)
  }
}

case class Power(e1: Expression, e2: Expression) extends Expression {
  override def toString: String = s"Power(${e1.toString},${e2.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp1 = e1.forward(varAssn, variable)
    val vp2 = e2.forward(varAssn, variable)
    val value = Math.pow(vp1.value, vp2.value)
    val partial = value * (vp2.value / vp1.value * vp1.partial + Math.log(
      vp1.value
    ) * vp2.partial)
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val e1Value = Process.eval(e1, varAssn)
    val e2Value = Process.eval(e2, varAssn)
    ValueOf.UpdateValue(e1.toString, e1Value)
    ValueOf.UpdateValue(e2.toString, e2Value)
    e1.reverse(
      seed * Math.pow(e1Value, e2Value) * e2Value / e1Value,
      varAssn
    )
    e2.reverse(
      seed * Math.pow(e1Value, e2Value) * Math.log(e1Value),
      varAssn
    )

  }
}

case class Sin(e: Expression) extends Expression {
  override def toString: String = s"Sin(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.sin(vp.value)
    val partial = Math.cos(vp.value) * vp.partial
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(seed * Math.cos(eValue), varAssn)

  }
}

case class Cos(e: Expression) extends Expression {
  override def toString: String = s"Cos(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.cos(vp.value)
    val partial = -Math.sin(vp.value) * vp.partial
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(seed * -Math.sin(eValue), varAssn)
  }

}

case class Tan(e: Expression) extends Expression {
  override def toString: String = s"Tan(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.tan(vp.value)
    val partial = vp.partial / Math.pow(Math.cos(vp.value), 2)
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(seed / Math.pow(Math.cos(eValue), 2), varAssn)
  }

}

case class Sec(e: Expression) extends Expression {
  override def toString: String = s"Sec(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = 1 / Math.cos(vp.value)
    val partial = vp.partial * value * Math.tan(vp.value) // Corrected here
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(seed * 1 / Math.cos(eValue) * Math.tan(eValue), varAssn) // Corrected here
  }
}

case class Csc(e: Expression) extends Expression {
  override def toString: String = s"Csc(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = 1 / Math.sin(vp.value)
    val partial = -vp.partial * value * 1 / Math.tan(vp.value) // Corrected here
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(-seed * 1 / Math.sin(eValue) * 1 / Math.tan(eValue), varAssn) // Corrected here
  }
}

case class Cot(e: Expression) extends Expression {
  override def toString: String = s"Cot(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = 1 / Math.tan(vp.value)
    val partial = -vp.partial / Math.sin(vp.value) * vp.partial / Math.sin(vp.value)
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(-seed / Math.pow(Math.sin(eValue), 2), varAssn)
  }
}

case class ArcSin(e: Expression) extends Expression {
  override def toString: String = s"ArcSin(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.asin(vp.value)
    val partial = vp.partial / Math.sqrt(1 - Math.pow(vp.value, 2))
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(seed / Math.sqrt(1 - Math.pow(eValue, 2)), varAssn)
  }
}

case class ArcCos(e: Expression) extends Expression {
  override def toString: String = s"ArcCos(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.acos(vp.value)
    val partial = -vp.partial / Math.sqrt(1 - Math.pow(vp.value, 2))
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(-seed / Math.sqrt(1 - Math.pow(eValue, 2)), varAssn)
  }
}

case class ArcTan(e: Expression) extends Expression {
  override def toString: String = s"ArcTan(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.atan(vp.value)
    val partial = vp.partial / (1 + Math.pow(vp.value, 2))
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(seed / (1 + Math.pow(eValue, 2)), varAssn)
  }
}

case class ArcSec(e: Expression) extends Expression {
  override def toString: String = s"ArcSec(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.acos(1 / vp.value)
    val partial = vp.partial / (Math.abs(vp.value) * Math.sqrt(Math.pow(vp.value, 2) - 1))
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(seed / (Math.abs(eValue) * Math.sqrt(Math.pow(eValue, 2) - 1)), varAssn)
  }
}

case class ArcCsc(e: Expression) extends Expression {
  override def toString: String = s"ArcCsc(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.asin(1 / vp.value)
    val partial = -vp.partial / (Math.abs(vp.value) * Math.sqrt(Math.pow(vp.value, 2) - 1))
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(-seed / (Math.abs(eValue) * Math.sqrt(Math.pow(eValue, 2) - 1)), varAssn)
  }
}

case class ArcCot(e: Expression) extends Expression {
  override def toString: String = s"ArcCot(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.atan(1 / vp.value)
    val partial = -vp.partial / (1 + Math.pow(vp.value, 2))
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(-seed / (1 + Math.pow(eValue, 2)), varAssn)
  }
}

case class Ln(e: Expression) extends Expression {
  override def toString: String = s"Ln(${e.toString})"

  override def forward(
    varAssn: Map[String, Double],
    variable: String
  ): ValueAndPartial = {
    val vp = e.forward(varAssn, variable)
    val value = Math.log(vp.value)
    val partial = vp.partial / vp.value
    ValueAndPartial(value, partial)
  }

  override def reverse(seed: Double, varAssn: Map[String, Double]): Unit = {
    val eValue = Process.eval(e, varAssn)
    ValueOf.UpdateValue(e.toString, eValue)
    e.reverse(seed / eValue, varAssn)
  }
}
