package solver

import general._
import processor.{AuxProcessor, BigProcessor}
import solver.Status._

import scala.collection.immutable.HashSet

class SolutionCase(cases: List[String], inputEquations: HashSet[FunEqEquation], assumptions: List[FunEqEquation]) {

  private val maxDepth = 3
  private var equations: Option[HashSet[FunEqEquation]] = None
  private var children: List[SolutionCase] = Nil
  private var status: Status = Status.Unsolved
  private var solutions: HashSet[FunEqEquation] = HashSet()

  def solve(): Unit = {

    println(s"\nCase: $name - start")

    val processedEquations = new BigProcessor(assumptions)
      .process(inputEquations ++ assumptions)

    println(s"Case $name - end :: ${processedEquations.size}")

    equations = Some(processedEquations)

    if(processedEquations.exists(_.isImpossible)) status = Status.Impossible
    else {
      val sols = processedEquations.filter(_.isSolution)

      if (sols.isEmpty) {

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
                      y._1)))

            case None =>
          }

          for (child <- children) child.solve()

          solutions = HashSet() ++ children.flatMap(_.solutions)
        }
      }
      else {
        solutions = sols
        status = Status.Solved
      }
    }

    if (children.nonEmpty) {
      if (children.forall(_.status == Status.Impossible)) status = Status.Impossible
      else if (!children.exists(_.status == Status.Unsolved)) status = Status.Solved
    }
  }

  def name: String = nameFromCases(cases)

  def nameFromCases(cases: List[String]): String = {
    cases.mkString("-") match {
      case "" => "Main"
      case other => other
    }
  }

  def print(detailed: Boolean): Unit = {
    println(s"Case: $name - $status")

    // ToDo: Simplify assumptions here, just for printing
    assumptions match {
      case Nil => println("\tNo assumptions.")
      case _ => println(s"\tAssumptions: ${assumptions.mkString(", ")}")
    }

    println()

    solutions.size match {
      case 0 =>
      case 1 => println(s"\tSolution: ${solutions.head}\n")
      case _ =>
        println("\tSolutions:")
        for (s <- solutions) println(s"\t\t$s")
        println()
    }

    equations match {
      case None => println("Not processed yet.")
      case Some(e) =>
        println(s"\tTotal equations: ${e.size}\n")
        for (equation <- e.toList.sortWith(_.toString.length < _.toString.length))
          equation.print(detailed, prefix = "\t\t")
    }

    for (child <- children) {
      println()
      child.print(detailed)
    }
  }

  override def toString: String = name
}
