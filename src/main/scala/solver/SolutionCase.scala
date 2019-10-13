package solver

import general.{FunEqEquation, FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode, FunEqSource, FunEqVarLeaf, Info}
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

    val processedEquations = new BigProcessor(assumptions)
      .process(inputEquations ++ assumptions)

    equations = Some(processedEquations)

    if(processedEquations.exists(_.isImpossible)) status = Status.Impossible
    else {
      val sols = processedEquations.filter(isSolution)

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

  private def isSolution(equation: FunEqEquation): Boolean = {
    equation match {
      case FunEqEquation(_, FunEqVarLeaf("x"), right, true) if xOnly(right) => true
      case _ => false
    }
  }

  private def xOnly(expression: FunEqExpression): Boolean =
      (Info.getAllVariables(expression) diff HashSet("x")).isEmpty && !hasFunctionCalls(expression)

  private def hasFunctionCalls(expression: FunEqExpression): Boolean = {
    expression match {
      case FunEqFunc(_, _) => true
      case FunEqNode(_, left, right) => hasFunctionCalls(left) || hasFunctionCalls(right)
      case _ => false
    }
  }

  override def toString: String = name
}
