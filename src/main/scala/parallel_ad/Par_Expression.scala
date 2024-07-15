package parallel_ad

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

sealed trait Par_Expression {
  def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial]
  def backward(seed: Double, varAssn: Map[String, Double]): Future[Unit]

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
  var grads: scala.collection.concurrent.Map[String, Double] =
    new scala.collection.concurrent.TrieMap()

  def safeUpdateGrad(name: String, value: Double): Unit = this.synchronized {
    grads.updateWith(name) {
      case Some(currentValue) => Some(currentValue + value)
      case None               => Some(value)
    }
  }
}

object ValueOf {
  var values: scala.collection.concurrent.Map[String, Double] =
    new scala.collection.concurrent.TrieMap()

  def safeUpdateValue(name: String, value: Double): Unit = this.synchronized {
    values.updateWith(name) {
      case Some(currentValue) => Some(currentValue + value)
      case None               => Some(value)
    }
  }
}

case class ValueAndPartial(value: Double, partial: Double) {
  def toList: List[Double] = List(value, partial)
}

case class Constant(n: Double) extends Par_Expression {
  override def toString: String = s"Constant($n)"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    Future {
      ValueAndPartial(n, 0)
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    Future.successful(())
  }
}

case class Var(name: String) extends Par_Expression {
  override def toString: String = s"Var($name)"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    Future {
      val value = varAssn.getOrElse(name, 0.0)
      ValueAndPartial(value, if (name == variable) 1 else 0)
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    Future {
      PartialDerivativeOf.safeUpdateGrad(name, seed)
    }
  }
}

case class Sum(e1: Par_Expression, e2: Par_Expression) extends Par_Expression {
  override def toString: String = s"Sum(${e1.toString},${e2.toString})"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    e1.forward(varAssn, variable).flatMap { vp1 =>
      e2.forward(varAssn, variable).map { vp2 =>
        ValueAndPartial(vp1.value + vp2.value, vp1.partial + vp2.partial)
      }
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    e1.backward(seed, varAssn).flatMap { _ =>
      e2.backward(seed, varAssn)
    }
  }
}

case class Prod(e1: Par_Expression, e2: Par_Expression) extends Par_Expression {
  override def toString: String = s"Prod(${e1.toString},${e2.toString})"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    e1.forward(varAssn, variable).flatMap { vp1 =>
      e2.forward(varAssn, variable).map { vp2 =>
        ValueAndPartial(
          vp1.value * vp2.value,
          vp1.value * vp2.partial + vp1.partial * vp2.value
        )
      }
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    val e1Value = Par_Process.eval(e1, varAssn)
    val e2Value = Par_Process.eval(e2, varAssn)

    val grad1 = seed * e2Value
    val grad2 = seed * e1Value

    e1.backward(grad1, varAssn).flatMap { _ =>
      e2.backward(grad2, varAssn)
    }
  }
}

case class Power(e1: Par_Expression, e2: Par_Expression)
    extends Par_Expression {
  override def toString: String = s"Power(${e1.toString},${e2.toString})"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    e1.forward(varAssn, variable).flatMap { vp1 =>
      e2.forward(varAssn, variable).map { vp2 =>
        ValueAndPartial(
          Math.pow(vp1.value, vp2.value),
          Math.pow(vp1.value, vp2.value - 1) * vp2.value * vp1.partial +
            Math.pow(vp1.value, vp2.value) * Math.log(vp1.value) * vp2.partial
        )
      }
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    val e1Value = Par_Process.eval(e1, varAssn)
    val e2Value = Par_Process.eval(e2, varAssn)

    val grad1 = seed * e2Value * Math.pow(e1Value, e2Value - 1)
    val grad2 = seed * Math.pow(e1Value, e2Value) * Math.log(e1Value)

    e1.backward(grad1, varAssn).flatMap { _ =>
      e2.backward(grad2, varAssn)
    }
  }
}

case class Sin(e: Par_Expression) extends Par_Expression {
  override def toString: String = s"Sin(${e.toString})"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    e.forward(varAssn, variable).map { vp =>
      ValueAndPartial(Math.sin(vp.value), Math.cos(vp.value) * vp.partial)
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    val eValue = Par_Process.eval(e, varAssn)
    e.backward(seed * Math.cos(eValue), varAssn)
  }
}

case class Cos(e: Par_Expression) extends Par_Expression {
  override def toString: String = s"Cos(${e.toString})"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    e.forward(varAssn, variable).map { vp =>
      ValueAndPartial(Math.cos(vp.value), -Math.sin(vp.value) * vp.partial)
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    val eValue = Par_Process.eval(e, varAssn)
    e.backward(-seed * Math.sin(eValue), varAssn)
  }
}

case class Tan(e: Par_Expression) extends Par_Expression {
  override def toString: String = s"Tan(${e.toString})"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    e.forward(varAssn, variable).map { vp =>
      ValueAndPartial(
        Math.tan(vp.value),
        Math.pow(1 / Math.cos(vp.value), 2) * vp.partial
      )
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    val eValue = Par_Process.eval(e, varAssn)
    e.backward(seed / Math.pow(Math.cos(eValue), 2), varAssn)
  }
}

case class Ln(e: Par_Expression) extends Par_Expression {
  override def toString: String = s"Ln(${e.toString})"

  override def forward(
      varAssn: Map[String, Double],
      variable: String
  ): Future[ValueAndPartial] = {
    e.forward(varAssn, variable).map { vp =>
      ValueAndPartial(Math.log(vp.value), vp.partial / vp.value)
    }
  }

  override def backward(
      seed: Double,
      varAssn: Map[String, Double]
  ): Future[Unit] = {
    val eValue = Par_Process.eval(e, varAssn)
    e.backward(seed / eValue, varAssn)
  }
}
