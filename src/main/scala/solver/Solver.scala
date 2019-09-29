package solver

import general.FunEqEquation

import scala.collection.immutable.HashSet

object Solver {

  private def expand(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] = {
    def newEquations = equations ++ equations.flatMap(x => expandSingle(x))

    if (newEquations.size > equations.size) expand(newEquations) else newEquations
  }

  def expand(equation: FunEqEquation): HashSet[FunEqEquation] = expand(HashSet(Simplifier.simplify(equation)))

  private def expandSingle(equation: FunEqEquation) = {
    for (
      variable <- Substitutor.allVariables(equation);
      value <- -1 to 2
    ) yield subAndSimplify(equation, variable, value)
  }

  private def subAndSimplify(equation: FunEqEquation, variable: String, value: Int): FunEqEquation = {
    val newEquation = Simplifier.simplify(Substitutor.sub(equation, variable, value))
    if (newEquation.toString == "2 * f(y) = f(0) + f(2 * y)")
      println(":: " + equation + " --> " + newEquation + " [" + variable + "=" + value + "]")
    newEquation
  }

  def printTrace(allEquations: HashSet[FunEqEquation], derivedEquation: String): Unit = {
    val firstEq = findByText(derivedEquation, allEquations)
    val trace = getTrace(firstEq, allEquations, List())

    println("Trace length: " + trace.length)

    for (
      equation <- trace.reverse
    ) println(equation)
  }

  def getTrace(
      currEquation: Option[FunEqEquation],
      allEquations: HashSet[FunEqEquation],
      trace: List[FunEqEquation]): List[FunEqEquation]= {

    currEquation match {
      case Some(equation) if equation.parents.nonEmpty =>
        getTrace(findByGuid(equation.parents.head, allEquations), allEquations, trace :+ equation)

      case Some(equation) if equation.parents.isEmpty => trace :+ equation

      case None => trace
    }
  }

  def findByGuid(guid: String, allEquations: HashSet[FunEqEquation]): Option[FunEqEquation] =
    allEquations.find(_.guid == guid)

  def findByText(equation: String, allEquations: HashSet[FunEqEquation]): Option[FunEqEquation] =
    allEquations.find(_.toString == equation)
}


