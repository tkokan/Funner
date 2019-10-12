package processor.simplifier

import general.{FunEqEquation, FunEqExpression, FunEqIntLeaf}
import scala.annotation.tailrec

// ToDo: MultiplicationSimplifier and CancelMultiplicationSimplifier need to be merged
object MultiplicationSimplifier extends AssociativeOperationSimplifier with Simplifier {

  override protected val operation: String = "*"
  override protected val neturalElement: Int = 1

  override def combineTwo(x: FunEqExpression, y: FunEqExpression): Option[FunEqExpression] = (x, y) match {
    case (FunEqIntLeaf(0), _) => Some(FunEqIntLeaf(0))
    case (_, FunEqIntLeaf(0)) => Some(FunEqIntLeaf(0))
    case (FunEqIntLeaf(a), FunEqIntLeaf(b)) => Some(FunEqIntLeaf(a * b))
    //case (FunEqNegation(FunEqIntLeaf(a)), _) => combineTwo(FunEqIntLeaf(-a), y)
    //case (_, FunEqNegation(FunEqIntLeaf(b))) => combineTwo(x, FunEqIntLeaf(-b))
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

//package processor.simplifier
//
//import general._
//import processor.IndependentProcessor
//
//import scala.collection.immutable.HashSet
//
//object MultiplicationSimplifier extends Simplifier {
//
//  final override def process(equation: FunEqEquation): FunEqEquation = {
//    val source = FunEqSource(List(equation), "Cancel multiplication.")
//
//      equation match {
//        case FunEqEquation(_, FunEqNode("*", FunEqIntLeaf(a), x), FunEqNode("*", FunEqIntLeaf(b), y)) if a == b && a != 0
//        => FunEqEquation(source, x, y)
//
//        case FunEqEquation(_, FunEqNode("*", FunEqIntLeaf(a), x), FunEqNode("*", y, FunEqIntLeaf(b))) if a == b && a != 0
//        => FunEqEquation(source, x, y)
//
//        case FunEqEquation(_, FunEqNode("*", x, FunEqIntLeaf(a)), FunEqNode("*", FunEqIntLeaf(b), y)) if a == b && a != 0
//        => FunEqEquation(source, x, y)
//
//        case FunEqEquation(_, FunEqNode("*", x, FunEqIntLeaf(a)), FunEqNode("*", y, FunEqIntLeaf(b))) if a == b && a != 0
//        => FunEqEquation(source, x, y)
//
//        case other => other
//      }
//  }
//}
