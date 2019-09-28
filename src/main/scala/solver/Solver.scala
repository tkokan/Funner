package solver

import parser.FunEqEquation

object Solver {

  private def expand(equations: Set[FunEqEquation]): Set[FunEqEquation] = {
    def newEquations = equations ++ equations.flatMap(x => expandSingle(x))

    if (newEquations.size > equations.size) expand(newEquations) else newEquations
  }

  def expand(equation: FunEqEquation): Set[FunEqEquation] = expand(Set(Simplifier.simplify(equation)))

  private def expandSingle(equation: FunEqEquation) = {
    for (
      variable <- Substitutor.allVariables(equation);
      value <- -1 to 2
    ) yield Simplifier.simplify(Substitutor.sub(equation, variable, value))
  }

  def printTrace(initialEquation: FunEqEquation, derivedEquation: String): Unit = {
    val allEquations = expand(initialEquation)
    val firstEq = findByGuid(derivedEquation, allEquations)
    val trace = getTrace(firstEq, allEquations, List())

    for (
      eq <- trace.reverse
    ) println(eq)
  }

  def getTrace(
      currEquation: FunEqEquation,
      allEquations: Set[FunEqEquation],
      trace: List[FunEqEquation]): List[FunEqEquation]= {

    currEquation.parents match {
      case List() => trace :+ currEquation
      case List(g) => getTrace(findByGuid(g, allEquations), allEquations, trace :+ currEquation)
    }
  }

  def findByGuid(guid: String, allEquations: Set[FunEqEquation]): FunEqEquation = ???
}


