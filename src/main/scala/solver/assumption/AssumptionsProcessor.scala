//package solver.assumption
//
//import general.{FunEqEquation, FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode, Info}
//
//import scala.collection.immutable.HashSet
//
//object AssumptionsProcessor {
//
//  def getBestAssumption(equations: HashSet[FunEqEquation]): Option[FunEqExpression] = {
//      equations.flatMap(getAssumptions) match {
//      case x if x.isEmpty => None
//      case assumptions => Some(assumptions.groupBy(identity).maxBy(_._2.size)._1)
//    }
//  }
//
//  def allNonzeroAssumptions(assumptions: HashSet[Assumption]): HashSet[FunEqExpression] = {
//    assumptions.filter(_.assumptionType == AssumptionType.IsNotZero).map(_.expression)
//  }
//
//  private def constantsOnly(expression: FunEqExpression): Boolean = Info.getAllVariables(expression).isEmpty
//
//  private def getAssumptions(equation: FunEqEquation): HashSet[FunEqExpression] = equation match {
//    case FunEqEquation(_, FunEqNode("*", _, _), FunEqIntLeaf(0)) => getConstantFactors(equation.left)
//    // case FunEqEquation(_, FunEqIntLeaf(0), FunEqNode("*", _, _)) => getConstantFactors(equation.right)
//    case FunEqEquation(_, left, right) => getConstantFactors(left) intersect getConstantFactors(right)
//  }
//
//  private def getConstantFactors(expression: FunEqExpression): HashSet[FunEqExpression] = expression match {
//    case FunEqNode("*", left, right) => getConstantFactors(left) ++ getConstantFactors(right)
//    case FunEqFunc(_, argument) if constantsOnly(argument) => HashSet(expression)
//    case _ => HashSet()
//  }
//}
