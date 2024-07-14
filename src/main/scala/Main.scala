//import parallel_ad.{Par_AutoDiff, Par_Parser, Par_Process, PartialDerivativeOf}
//import sequential_ad.{AutoDiff => Seq_AutoDiff, Parser => Seq_Parser, Process => Seq_Process, PartialDerivativeOf => Seq_PartialDerivativeOf}
//
//object Main {
//  def main(args: Array[String]): Unit = {
//    println("Enter an expression:")
//    val expString = scala.io.StdIn.readLine()
//
//    println("Enter variable names (comma separated):")
//    val varNames = scala.io.StdIn.readLine().split(",").map(_.trim).toList
//
//    println("Enter corresponding variable values (comma separated):")
//    val varValues = scala.io.StdIn.readLine().split(",").map(_.trim.toDouble).toList
//
//    val varAssn = varNames.zip(varValues).toMap
//
//    println("Choose mode (1 for forward, 2 for reverse):")
//    val mode = scala.io.StdIn.readInt()
//
//    mode match {
//      case 1 =>
//        val parExp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
//        val seqExp = Seq_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
//        println("Derivatives in forward mode:")
//        varNames.foreach { variable =>
//          val parPartial = Par_AutoDiff.forwardMode(parExp, varAssn, variable)
//          val seqPartial = Seq_AutoDiff.forwardMode(seqExp, varAssn, variable)
//          println(s"Parallel derivative with respect to $variable: ${parPartial.toList}")
//          println(s"Sequential derivative with respect to $variable: ${seqPartial.toList}")
//        }
//      case 2 =>
//        val parExp = Par_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
//        val seqExp = Seq_Parser(expString).getOrElse(throw new Exception("Invalid expression"))
//        println("Derivatives in reverse mode:")
//        Par_AutoDiff.reverseMode(parExp, varAssn)
//        Seq_AutoDiff.reverseMode(seqExp, varAssn)
//        println("Parallel Gradients: " + PartialDerivativeOf.grads)
//        println("Sequential Gradients: " + Seq_PartialDerivativeOf.grads)
//      case _ =>
//        println("Invalid mode selected.")
//    }
//  }
//}