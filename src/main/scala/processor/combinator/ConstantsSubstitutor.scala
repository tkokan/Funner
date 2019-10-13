package processor.combinator

import general._
import processor.IndependentProcessor
import processor.simplifier.BigSimplifier

import scala.collection.immutable.HashSet


// ToDo: This should be using the other equation substitution
object ConstantsSubstitutor extends IndependentProcessor {

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
      case FunEqEquation(_, left, right, isEquality) =>
        FunEqEquation(
          FunEqSource(s"Substitution [$variable <- $value] in $equation", List(equation)),
          sub(left, variable, value),
          sub(right, variable, value),
          isEquality)
    }
  }

  private def sub(expr: FunEqExpression, variable: String, value: Int): FunEqExpression = expr match {
    case FunEqVarLeaf(`variable`) => FunEqIntLeaf(value)
    case FunEqVarLeaf(name) => FunEqVarLeaf(name)
    case FunEqNode(op, left, right) => FunEqNode(op, sub(left, variable, value), sub(right, variable, value))
    case FunEqFunc(name, argument) => FunEqFunc(name, sub(argument, variable, value))
    case FunEqIntLeaf(v) => FunEqIntLeaf(v)
  }

  private def subAndSimplify(equation: FunEqEquation, variable: String, value: Int): FunEqEquation =
    new BigSimplifier().simplify(sub(equation, variable, value))
}
