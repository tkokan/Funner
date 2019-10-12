package solver

import general.{FunEqEquation, FunEqIntLeaf, FunEqSource}
import processor.{AuxProcessor, MainProcessor}
import solver.Status._

import scala.collection.immutable.HashSet

class SolutionCase(cases: List[String], inputEquations: HashSet[FunEqEquation], assumptions: List[FunEqEquation]) {

  private val maxDepth = 3
  private var equations: Option[HashSet[FunEqEquation]] = None
  private var children: List[SolutionCase] = Nil

  def status: Status = Status.Unsolved

  def solve(): Unit = {

    val processedEquations = new MainProcessor(assumptions)
      .process(inputEquations ++ assumptions)

    equations = Some(processedEquations)

    if (cases.length < maxDepth) {
      val assumption = AuxProcessor.getBestAssumption(processedEquations)

      assumption match {
        case Some(x) =>
          children = List(true, false)
            .zipWithIndex
            .map(y =>
              new SolutionCase(
                cases :+ (y._2 + 1).toString,
                processedEquations,
                assumptions :+ FunEqEquation(
                  FunEqSource(description = s"Assumption for Case ${nameFromCases(cases :+ (y._2 + 1).toString)}."),
                  x,
                  FunEqIntLeaf(0),
                  y._1
                )
              )
            )

        case None =>
      }

      for (child <- children) child.solve()
    }
  }

  def name: String = nameFromCases(cases)

  def nameFromCases(cases: List[String]): String = cases.mkString("-") match {
    case "" => "Main"
    case other => other
  }

  def print(detailed: Boolean): Unit = {
    println(s"Case: $name")

    assumptions match {
      case Nil => println("\tNo assumptions.")
      case _ => println(s"\tAssumptions: ${assumptions.mkString(", ")}")
    }

    println()
    equations match {
      case None => println("Not processed yet.")
      case Some(e) =>
        println(s"Total equations: ${e.size}\n")
        for (equation <- e.toList.sortWith(_.toString.length < _.toString.length)) equation.print(detailed)
    }

    for (child <- children) {
      println()
      child.print(detailed)
    }
  }

  override def toString: String = name
}
