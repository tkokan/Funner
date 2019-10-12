package processor

import general.{FunEqEquation, FunEqExpression, FunEqSource}

abstract class PiecewiseProcessor extends SingleResultProcessor {

  val description: String

  final override def processOneResult(equation: FunEqEquation): FunEqEquation = equation match {
    case FunEqEquation(_, left, right, _)
    => FunEqEquation(equation.source, process(left), process(right), equation.isEquality)
  }

  def process(expression: FunEqExpression): FunEqExpression
}
