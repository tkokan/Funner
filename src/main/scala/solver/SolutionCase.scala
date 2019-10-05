package solver

import general.FunEqEquation
import StatusEnum._
import processor.MainProcessor
import scala.collection.immutable.HashSet

class SolutionCase(cases: List[String], inputEquations: HashSet[FunEqEquation], assumptions: List[Assumption]) {

  private var equations: Option[HashSet[FunEqEquation]] = None

  def status: Status = StatusEnum.Unsolved

  def solve(): Unit = {
    equations = Some(MainProcessor.process(inputEquations))
  }

  def name: String = {
    cases.mkString("-") match {
      case "" => "Main"
      case other => other
    }
  }

  def print(): Unit = {
    println(s"Case: $name")

    println()
    equations match {
      case None => println("Not processed yet.")
      case Some(e) =>
        println(s"Total equations: ${e.size}\n")
        for (equation <- e.toList.sortWith(_.toString.length < _.toString.length)) println(equation)
    }
  }

  override def toString: String = name
}
