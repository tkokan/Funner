package processor.simplifier

import general._
import processor.AuxProcessor.{aggregate, getOperands}

import scala.annotation.tailrec

class CancelMultiplicationSimplifier(assumptions: List[FunEqEquation]) extends Simplifier {

  final override def simplify(equation: FunEqEquation): FunEqEquation = {
    val (intsLhs, otherLhs) = getOperands(BinaryOperation.*, equation.left).partition(isIntLeaf)
    val (intsRhs, otherRhs) = getOperands(BinaryOperation.*, equation.right).partition(isIntLeaf)

    val productLhs = intsLhs.map({ case FunEqIntLeaf(v) => v }).product
    val productRhs = intsRhs.map({ case FunEqIntLeaf(v) => v }).product

    val d = gcd(math.abs(productLhs), math.abs(productRhs))

    val (canceledLhs, canceledRhs, assumptionsUsed) = cancel(otherLhs, otherRhs, productLhs, productRhs)

    if (d == 0)
      equation
    else {

      val source = assumptionsUsed match {
        case Nil => equation.source
        case _ => FunEqSource(s"Cancel multiplication in ${equation}.", equation :: assumptionsUsed)
      }

      FunEqEquation(
        source,
        getExpression(productLhs / d, canceledLhs),
        getExpression(productRhs / d, canceledRhs),
        equation.isEquality
      )
    }
  }

  private def getExpression(constant: Int, expressions: List[FunEqExpression]): FunEqExpression = constant match {
    case 0 => FunEqIntLeaf(0)
    case 1 => aggregate(BinaryOperation.*, expressions)
    case a => aggregate(BinaryOperation.*, FunEqIntLeaf(a) :: expressions)
    //case a if a < 0 => FunEqNegation(getExpression(-a, expressions))
  }

  private def cancel(lhs: List[FunEqExpression], rhs: List[FunEqExpression], productLhs: Int, productRhs: Int):
  (List[FunEqExpression], List[FunEqExpression], List[FunEqEquation]) = {

    // integers have already been factored out
    val common = (lhs, rhs, productLhs, productRhs) match {
      case (_, _, 0, _) => rhs
      case (_, _, _, 0) => lhs
      case _ => lhs intersect rhs
    }

    val assumptionsToUse = assumptions.filter(x => common.contains(x.left) && !x.isEquality)
    val expressionsToBeCanceled = assumptionsToUse.map(_.left)

    (lhs diff expressionsToBeCanceled, rhs diff expressionsToBeCanceled, assumptionsToUse)
  }

  private def isIntLeaf(expression: FunEqExpression): Boolean = expression match {
    case FunEqIntLeaf(_) => true
    case _ => false
  }

  @tailrec
  private def gcd(a: Int,b: Int): Int = b match {
    case 0 => a
    case _ => gcd(b, a%b)
  }
}
