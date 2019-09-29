package simplifier

import general.{FunEqEquation, FunEqExpression, FunEqSource}

abstract class AbstractSimplifier {

  val description: String

  def simplify(equation: FunEqEquation): FunEqEquation =
    equation match {
      case FunEqEquation(_, left, right) =>

        FunEqEquation(
          FunEqSource(List(equation), description),
          simplify(left),
          simplify(right))
    }

  protected def simplify(expression: FunEqExpression): FunEqExpression = expression
}
