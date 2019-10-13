package processor.simplifier

import general.BinaryOperation.BinaryOperation
import general.{BinaryOperation, FunEqEquation, FunEqExpression, FunEqIntLeaf}

// ToDo: MultiplicationSimplifier and CancelMultiplicationSimplifier need to be merged
object MultiplicationSimplifier extends AssociativeOperationSimplifier with Simplifier {

  override protected val operation: BinaryOperation = BinaryOperation.*
  override protected val neturalElement: Int = 1

  override def combineTwo(x: FunEqExpression, y: FunEqExpression): Option[FunEqExpression] = (x, y) match {
    case (FunEqIntLeaf(0), _) => Some(FunEqIntLeaf(0))
    case (_, FunEqIntLeaf(0)) => Some(FunEqIntLeaf(0))
    case (FunEqIntLeaf(a), FunEqIntLeaf(b)) => Some(FunEqIntLeaf(a * b))
    case _ => None
  }

  override def simplify(equation: FunEqEquation): FunEqEquation
  = FunEqEquation(
    equation.source,
    simplifyExpression(equation.left),
    simplifyExpression(equation.right),
    equation.isEquality
  )
}
