//package solver.assumption
//
//import general.{FunEqEquation, FunEqExpression, FunEqIntLeaf, FunEqSource}
//import solver.assumption.AssumptionType.AssumptionType
//
//class Assumption(val expression: FunEqExpression, val assumptionType: AssumptionType) {
//
//  def toEquation: Option[FunEqEquation] = assumptionType match {
//    case AssumptionType.IsZero
//    => Some(FunEqEquation(FunEqSource(List(), "assumption"), expression, FunEqIntLeaf(0)))
//
//    case AssumptionType.IsNotZero => None
//  }
//
//  override def toString: String = assumptionType match {
//    case AssumptionType.IsZero => s"$expression = 0"
//    case AssumptionType.IsNotZero => s"$expression != 0"
//  }
//}
