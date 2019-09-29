package solver

import general.FunEqEquation

import scala.collection.immutable.HashSet

import simplifier.Simplifier

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
}


