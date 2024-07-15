import parallel_ad.Par_AutoDiff
import sequential_ad.AutoDiff as SequentialAutoDiff

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object Main {
  def timed[A](name: String)(f: => A): (Double, A) = {
    println(s"Running $name ...")
    Console.flush()
    val start = System.nanoTime
    val res = f
    val stop = System.nanoTime
    println("Done")
    Console.flush()
    ((stop - start) / 1e9, res)
  }

  def generateRandomExpression(variables: List[String], numOperations: Int): String = {
    val operations = List("+", "-", "*", "sin", "cos", "tan", "ln")
    val rand = new Random()

    def randomTerm(): String = {
      val varOrNum = if (rand.nextBoolean()) variables(rand.nextInt(variables.length)) else (rand.nextDouble() * 10).toString
      val op = operations(rand.nextInt(operations.length))
      op match {
        case "+" | "-" | "*" => s"$varOrNum $op ${randomTerm()}"
        case _ => s"$op($varOrNum)"
      }
    }

    (1 to numOperations).map(_ => randomTerm()).mkString(" + ")
  }

  def main(args: Array[String]): Unit = {
    println("Press 1 for manual input, 2 to go crazy :)")
    val choice = scala.io.StdIn.readInt()

    val (variableNames, expressionString, variableAssignments) = choice match {
      case 1 =>
        println("Enter variable names (comma separated):")
        val variableNames = scala.io.StdIn.readLine().split(",").map(_.trim).toList

        println("Enter an expression:")
        val expressionString = scala.io.StdIn.readLine()

        println("Enter corresponding variable values (comma separated):")
        val variableValues = scala.io.StdIn.readLine().split(",").map(_.trim.toDouble).toList

        val variableAssignments = variableNames.zip(variableValues).toMap
        (variableNames, expressionString, variableAssignments)

      case 2 =>
        println("Generating a random expression with 1000 operations...")
        val variableNames = List("x", "y", "z")
        val expressionString = generateRandomExpression(variableNames, 1000)
        val variableAssignments = Map("x" -> 1.0, "y" -> 1.0, "z" -> 1.0)
        (variableNames, expressionString, variableAssignments)

      case _ =>
        println("Invalid choice.")
        return
    }

    println(s"\nExpression: $expressionString")
    println(s"Variable Assignments: $variableAssignments")

    val sequentialExpression = expressionString
    val parallelExpression = expressionString


    val (tForSeq, resForSeq) = timed("Sequential Forward Mode") {
      SequentialAutoDiff.forwardMode(sequentialExpression, variableAssignments)
    }
    val (tRevSeq, resRevSeq) = timed("Sequential Reverse Mode") {
      SequentialAutoDiff.reverseMode(sequentialExpression, variableAssignments)
    }
    val (tForPar, resForPar) = timed("Parallel Forward Mode") {
      Par_AutoDiff.forwardMode(parallelExpression, variableAssignments)
    }
    val (tRevPar, resRevPar) = timed("Parallel Reverse Mode") {
      Par_AutoDiff.reverseMode(parallelExpression, variableAssignments)
    }

    println("\nSequential Mode (Forward):")
    println("===========================")
    println(f"Time taken: $tForSeq%.4f seconds")
    println(f"Result: $resForSeq")
    println("\nParallel Mode (Forward):")
    println("========================")
    println(f"Time taken: $tForPar%.4f seconds")
    println(f"Result: $resForPar")
    println("\nSequential Mode (Reverse):")
    println("===========================")
    println(f"Time taken: $tRevSeq%.4f seconds")
    println(f"Result: $resRevSeq")
    println("\nParallel Mode (Reverse):")
    println("========================")
    println(f"Time taken: $tRevPar%.4f seconds")
    println(f"Result: $resRevPar")
  }
}
