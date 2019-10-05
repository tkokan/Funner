package processor.simplifier

import general._
import processor.SingleResultProcessor

object AdditionSimplifier extends  SingleResultProcessor {

  override def processOneResult(equation: FunEqEquation): FunEqEquation = {
    val source = FunEqSource(List(equation), "Cancel addition.")

    equation match {
      case FunEqEquation(_, FunEqNode("+", l1, r1), FunEqNode("+", l2, r2)) if l1 == l2
      => FunEqEquation(source, r1, r2)

      case FunEqEquation(_, FunEqNode("+", l1, r1), FunEqNode("+", l2, r2)) if r1 == r2
      => FunEqEquation(source, l1, l2)

      case FunEqEquation(_, FunEqNode("+", l1, r1), FunEqNode("+", l2, r2)) if l1 == r2
      => FunEqEquation(source, r1, l2)

      case FunEqEquation(_, FunEqNode("+", l1, r1), FunEqNode("+", l2, r2)) if l2 == r1
      => FunEqEquation(source, r2, l1)

      case other => other
    }
  }
}
