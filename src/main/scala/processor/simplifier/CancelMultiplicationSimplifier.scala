package processor.simplifier

import general.{FunEqEquation, FunEqExpression, FunEqIntLeaf, FunEqSource}
import processor.AuxProcessor.{aggregate, getOperands}

import scala.annotation.tailrec

class CancelMultiplicationSimplifier(assumptions: List[FunEqEquation]) extends Simplifier {

  final override def simplify(equation: FunEqEquation): FunEqEquation = {
    val (intsLhs, otherLhs) = getOperands("*", equation.left).partition(isIntLeaf)
    val (intsRhs, otherRhs) = getOperands("*", equation.right).partition(isIntLeaf)

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

//      val source = FunEqSource(par)
//      if
//      } (somethingToCancel)
//        FunEqSource(List(equation), "Used assumptions.")
//      else
//        equation.source

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
    case 1 => aggregate("*", expressions, 1)
    case a => aggregate("*", FunEqIntLeaf(a) :: expressions, 1)
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

//  private def canBeCanceled(expression: FunEqExpression): Boolean = {
//    val neededAssumption = FunEqEquation(FunEqSource(), expression, FunEqIntLeaf(0), isEquality = false)
//    assumptions.contains(neededAssumption)
//  }

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


//package processor.simplifier
//
//import general._
//
//object CancelMultiplicationSimplifier extends Simplifier {
//
//  final override def simplify(equation: FunEqEquation): FunEqEquation = {
//    val source = FunEqSource(List(equation), "Cancel multiplication.")
//
//    equation match {
//      case FunEqEquation(_, FunEqNode("*", FunEqIntLeaf(a), x), FunEqNode("*", FunEqIntLeaf(b), y)) if a == b && a != 0
//      => FunEqEquation(source, x, y)
//
//      case FunEqEquation(_, FunEqNode("*", FunEqIntLeaf(a), x), FunEqNode("*", y, FunEqIntLeaf(b))) if a == b && a != 0
//      => FunEqEquation(source, x, y)
//
//      case FunEqEquation(_, FunEqNode("*", x, FunEqIntLeaf(a)), FunEqNode("*", FunEqIntLeaf(b), y)) if a == b && a != 0
//      => FunEqEquation(source, x, y)
//
//      case FunEqEquation(_, FunEqNode("*", x, FunEqIntLeaf(a)), FunEqNode("*", y, FunEqIntLeaf(b))) if a == b && a != 0
//      => FunEqEquation(source, x, y)
//
//      case other => other
//    }
//  }
//}
