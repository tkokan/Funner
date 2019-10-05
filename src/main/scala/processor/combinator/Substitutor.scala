package processor.combinator

import general._
import processor.IndependentProcessor
import scala.collection.immutable.HashSet

object Substitutor extends IndependentProcessor {

  override def process(equation: FunEqEquation): HashSet[FunEqEquation] = {
    val newEquations = {
        for (
          variable <- Info.getAllVariables(equation);
          value <- -1 to 2
        ) yield subAndSimplify(equation, variable, value)
    }

    newEquations + equation
  }

  private def sub(equation: FunEqEquation, variable: String, value: Int): FunEqEquation = {
    equation match {
      case FunEqEquation(_, left, right)
        => FunEqEquation(
          FunEqSource(List(equation), s"Substitution: $variable = $value"),
          sub(left, variable, value),
          sub(right, variable, value))
    }
  }

  private def sub(expr: FunEqExpression, variable: String, value: Int): FunEqExpression = expr match {
    case FunEqVarLeaf(`variable`) => FunEqIntLeaf(value)
    case FunEqVarLeaf(name) => FunEqVarLeaf(name)
    case FunEqNode(op, left, right) => FunEqNode(op, sub(left, variable, value), sub(right, variable, value))
    case FunEqFunc(name, argument) => FunEqFunc(name, sub(argument, variable, value))
    case FunEqIntLeaf(v) => FunEqIntLeaf(v)
  }

  private def subAndSimplify(equation: FunEqEquation, variable: String, value: Int): FunEqEquation = {
    val newEquation = sub(equation, variable, value)
    if (newEquation.toString == "2 * f(y) = f(0) + f(2 * y)")
      println(":: " + equation + " --> " + newEquation + " [" + variable + "=" + value + "]")
    newEquation
  }
}
