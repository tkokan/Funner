package processor.simplifier

import general.{FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode}
import processor.AuxProcessor.{aggregate, getOperands}

import scala.annotation.tailrec

abstract class AssociativeOperationSimplifier {

  protected val operation: String
  protected val neturalElement: Int

  protected def combineTwo(x: FunEqExpression, y: FunEqExpression): Option[FunEqExpression]

  protected def simplifyExpression(expression: FunEqExpression): FunEqExpression = expression match {
    case FunEqNode(op, _, _) if op == operation => simplifyAssociativeOperation(expression)
    case FunEqNode(op, a, b) => FunEqNode(op, simplifyExpression(a), simplifyExpression(b))
    case FunEqFunc(name, x) => FunEqFunc(name, simplifyExpression(x))
    case _ => expression
  }

  private def simplifyAssociativeOperation(expression: FunEqExpression): FunEqExpression = {
    val parts = getOperands(operation, expression).map(simplifyExpression)
    val processed = processOperands(parts)
    aggregate(operation, processed, neturalElement)
  }

  // Combine every pair of operands together
  // For example: (2, x, 4) -> (x, 6)
  protected def processOperands(operands: List[FunEqExpression]): List[FunEqExpression] = operands match {
    case a :: b :: rest => processOperands(a, b, List(), List(), rest)
    case _ => operands
  }
  @tailrec
  private def processOperands(
                       x: FunEqExpression,
                       y: FunEqExpression,
                       as: List[FunEqExpression],
                       bs: List[FunEqExpression],
                       cs: List[FunEqExpression]): List[FunEqExpression] = {

    combineTwoExtended(x, y) match {
      case Some(z) => processOperands(z :: as ++ bs ++ cs)
      case None => cs match {
        case d :: ds => processOperands(x, d, as, bs :+ y, ds)
        case Nil => bs match {
          case e1 :: es => processOperands(e1, y, as :+ x, List(), es)
          case _ => (as :+ x) ++ (bs :+ y) ++ cs
        }
      }
    }
  }

  private def combineTwoExtended(x: FunEqExpression, y: FunEqExpression): Option[FunEqExpression] = (x, y) match {
    case (FunEqIntLeaf(a), _) if a == neturalElement => Some(y)
    case (_, FunEqIntLeaf(b)) if b == neturalElement => Some(x)
    case _ => combineTwo(x, y)
  }
}
