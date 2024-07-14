
package parallel_ad

import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

sealed trait Par_Expression {
  def forward(varAssn: Map[String, Double], variable: String): ValueAndPartial

  def backward(seed: Double, varAssn: Map[String, Double]): Unit

  def findVar(name: String): Option[Var] = this match {
    case v: Var if v.name == name => Some(v)
    case Sum(e1, e2)              => e1.findVar(name).orElse(e2.findVar(name))
    case Prod(e1, e2)             => e1.findVar(name).orElse(e2.findVar(name))
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
  def safeUpdateGrad(name: String, value: Double): Unit = this.synchronized {
    grads(name) = grads.getOrElse(name, 0.0) + value
  }
}

object ValueOf {
  var values: scala.collection.mutable.Map[String, Double] =
    scala.collection.mutable.Map()
  def safeUpdateValue(name: String, value: Double): Unit = this.synchronized {
    values(name) = value
  }
}

case class ValueAndPartial(value: Double, partial: Double) {
  def toList: List[Double] = List(value, partial)
}

case class Constant(n: Double) extends Par_Expression {
  override def toString: String = s"Const($n)"

  override def forward(
                        varAssn: Map[String, Double],
                        variable: String
                      ): ValueAndPartial = {
    ValueAndPartial(n, 0)
  }

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {}
}

case class Var(name: String) extends Par_Expression {
  override def toString: String = s"Var($name)"

  override def forward(
                        varAssn: Map[String, Double],
                        variable: String
                      ): ValueAndPartial = {
    val value = varAssn(name)
    val partial = if (name == variable) 1 else 0
    ValueAndPartial(value, partial)
  }

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {
    PartialDerivativeOf.safeUpdateGrad(name, seed)
  }
}

case class Sum(e1: Par_Expression, e2: Par_Expression) extends Par_Expression {
  override def toString: String = s"Sum(${e1.toString}, ${e2.toString})"

  override def forward(
                        varAssn: Map[String, Double],
                        variable: String
                      ): ValueAndPartial = {
    val vp1 = e1.forward(varAssn, variable)
    val vp2 = e2.forward(varAssn, variable)
    println(s"Sum forward: vp1 = $vp1, vp2 = $vp2")
    ValueAndPartial(vp1.value + vp2.value, vp1.partial + vp2.partial)
  }

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {
    val e1Value = Par_Process.eval(e1, varAssn)
    ValueOf.safeUpdateValue(e1.toString, e1Value)
    val e2Value = Par_Process.eval(e2, varAssn)
    ValueOf.safeUpdateValue(e2.toString, e2Value)

    println(s"Sum backward: seed = $seed, e1Value = $e1Value, e2Value = $e2Value")

    e1.backward(seed, varAssn)
    e2.backward(seed, varAssn)
  }
}


case class Prod(e1: Par_Expression, e2: Par_Expression) extends Par_Expression {
  override def toString: String = s"Prod(${e1.toString}, ${e2.toString})"

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

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {
    val futures = Seq(
      Future {
        val e1Value = Par_Process.eval(e1, varAssn)
        ValueOf.safeUpdateValue(e1.toString, e1Value)
        val grad = seed * ValueOf.values.getOrElse(e2.toString, 0.0)
        println(s"e1 grad: $grad")
        e1.backward(grad, varAssn)
      },
      Future {
        val e2Value = Par_Process.eval(e2, varAssn)
        ValueOf.safeUpdateValue(e2.toString, e2Value)
        val grad = seed * ValueOf.values.getOrElse(e1.toString, 0.0)
        println(s"e2 grad: $grad")
        e2.backward(grad, varAssn)
      }
    )
    futures.foreach(f => Await.result(f, Duration.Inf))
  }
}

case class Power(e1: Par_Expression, e2: Par_Expression)
  extends Par_Expression {
  override def toString: String = s"Power(${e1.toString}, ${e2.toString})"

  override def forward(
                        varAssn: Map[String, Double],
                        variable: String
                      ): ValueAndPartial = {
    val vpB = e1.forward(varAssn, variable)
    val vpE = e2.forward(varAssn, variable)
    val value = Math.pow(vpB.value, vpE.value)
    val partial = value * (vpE.value / vpB.value * vpB.partial + Math.log(
      vpB.value
    ) * vpE.partial)
    ValueAndPartial(value, partial)
  }

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {
    val futures = Seq(
      Future {
        val e1Value = Par_Process.eval(e1, varAssn)
        ValueOf.safeUpdateValue(e1.toString, e1Value)
        e1.backward(
          seed * ValueOf.values.getOrElse(e1.toString, 0.0),
          varAssn
        )
      },
      Future {
        val e2Value = Par_Process.eval(e2, varAssn)
        ValueOf.safeUpdateValue(e2.toString, e2Value)
        e2.backward(
          seed * ValueOf.values.getOrElse(e1.toString, 0.0),
          varAssn
        )
      }
    )
    futures.foreach(f => Await.result(f, Duration.Inf))
  }
}

case class Sin(e: Par_Expression) extends Par_Expression {
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

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {
    val futures = Seq(
      Future {
        val eValue = Par_Process.eval(e, varAssn)
        ValueOf.safeUpdateValue(e.toString, eValue)
        e.backward(seed * Math.cos(eValue), varAssn)
      }
    )
    futures.foreach(f => Await.result(f, Duration.Inf))
  }
}

case class Cos(e: Par_Expression) extends Par_Expression {
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

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {
    val futures = Seq(
      Future {
        val eValue = Par_Process.eval(e, varAssn)
        ValueOf.safeUpdateValue(e.toString, eValue)
        e.backward(seed * -Math.sin(eValue), varAssn)
      }
    )
    futures.foreach(f => Await.result(f, Duration.Inf))
  }
}

case class Tan(e: Par_Expression) extends Par_Expression {
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

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {
    val futures = Seq(
      Future {
        val eValue = Par_Process.eval(e, varAssn)
        ValueOf.safeUpdateValue(e.toString, eValue)
        e.backward(seed / Math.pow(Math.cos(eValue), 2), varAssn)
      }
    )
    futures.foreach(f => Await.result(f, Duration.Inf))
  }

}

case class Ln(e: Par_Expression) extends Par_Expression {
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

  override def backward(seed: Double, varAssn: Map[String, Double]): Unit = {
    val futures = Seq(
      Future {
        val eValue = Par_Process.eval(e, varAssn)
        ValueOf.safeUpdateValue(e.toString, eValue)
        e.backward(seed / eValue, varAssn)
      }
    )
    futures.foreach(f => Await.result(f, Duration.Inf))
  }
}

