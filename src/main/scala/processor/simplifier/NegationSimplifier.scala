//package processor.simplifier
//
//import general.{FunEqExpression, FunEqIntLeaf}
//import processor.PiecewiseProcessor
//
//object NegationSimplifier  extends PiecewiseProcessor {
//
//  override val description: String = "Simplify negation."
//
//  override def process(expression: FunEqExpression): FunEqExpression = expression match {
//    case FunEqNegation(FunEqNegation(expr)) => expr
//    case FunEqNegation(FunEqIntLeaf(value)) => FunEqIntLeaf(-value)
//    case _ => expression
//  }
//}
