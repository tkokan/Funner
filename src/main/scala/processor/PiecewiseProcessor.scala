package processor

import general.{FunEqEquation, FunEqExpression, FunEqSource}

abstract class PiecewiseProcessor extends SingleResultProcessor {

  val description: String

  final override def processOneResult(equation: FunEqEquation): FunEqEquation = equation match {
    case FunEqEquation(_, left, right) =>
      FunEqEquation(
        FunEqSource(List(equation), description),
        process(left),
        process(right)
      )
  }

  def process(expression: FunEqExpression): FunEqExpression
}
