import parallel_ad.{Par_AutoDiff, Par_Parser, Par_Process, PartialDerivativeOf}
import sequential_ad.{AutoDiff as SequentialAutoDiff, Parser as SequentialParser, PartialDerivativeOf as SequentialPartialDerivativeOf, Process as SequentialProcess}

import scala.concurrent.Await
import scala.concurrent.duration._

object Main {
  def main(args: Array[String]): Unit = {

    println("Enter variable names (comma separated):")
    val variableNames = scala.io.StdIn.readLine().split(",").map(_.trim).toList

    println("Enter an expression:")
    val expressionString = scala.io.StdIn.readLine()

    println("Enter corresponding variable values (comma separated):")
    val variableValues = scala.io.StdIn.readLine().split(",").map(_.trim.toDouble).toList

    val variableAssignments = variableNames.zip(variableValues).toMap

    val sequentialExpression = expressionString
    val parallelExpression = expressionString

    println("\nSequential Mode (Forward):")
    println("===========================")
    val sequentialForwardStart = System.nanoTime()
    val sequentialForwardResult = SequentialAutoDiff.forwardMode(sequentialExpression, variableAssignments).toString()
    val sequentialForwardDuration = (System.nanoTime() - sequentialForwardStart) / 1e9d
    println(f"Result: $sequentialForwardResult")
    println(f"Time taken: $sequentialForwardDuration%.4f seconds\n")

    println("Parallel Mode (Forward):")
    println("========================")
    val parallelForwardStart = System.nanoTime()
    val forwardResultFuture = Par_AutoDiff.forwardMode(parallelExpression, variableAssignments)
    val parallelForwardResult = Await.result(forwardResultFuture, Duration.Inf)
    val parallelForwardDuration = (System.nanoTime() - parallelForwardStart) / 1e9d
    println(f"Result: $parallelForwardResult")
    println(f"Time taken: $parallelForwardDuration%.4f seconds\n")

    println("\nSequential Mode (Reverse):")
    println("===========================")
    val sequentialReverseStart = System.nanoTime()
    val sequentialReverseResult = SequentialAutoDiff.reverseMode(sequentialExpression, variableAssignments).toString()
    val sequentialReverseDuration = (System.nanoTime() - sequentialReverseStart) / 1e9d
    println(f"Result: $sequentialReverseResult")
    println(f"Time taken: $sequentialReverseDuration%.4f seconds\n")

    println("Parallel Mode (Reverse):")
    println("========================")
    val parallelReverseStart = System.nanoTime()
    val reverseResultFuture = Par_AutoDiff.reverseMode(parallelExpression, variableAssignments)
    val parallelReverseResult = Await.result(reverseResultFuture, Duration.Inf)
    val parallelReverseDuration = (System.nanoTime() - parallelReverseStart) / 1e9d
    println(f"Result: $parallelReverseResult")
    println(f"Time taken: $parallelReverseDuration%.4f seconds\n")
  }
}