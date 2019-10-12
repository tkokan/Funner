package processor

import general.{FunEqEquation, FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode, Info}

import scala.collection.immutable.HashSet

object AuxProcessor {
  def getOperands(operation: String, expression: FunEqExpression): List[FunEqExpression] = expression match {
    case FunEqNode(op, left, right) if op == operation => getOperands(op, left) ++ getOperands(op, right)
    case _ => List(expression)
  }

  def aggregate(
                 operation: String,
                 parts: List[FunEqExpression],
                 defaultValue: Int): FunEqExpression = parts match {

    case Nil => FunEqIntLeaf(defaultValue)
    case List(x) => x
    case x :: xs => FunEqNode(operation, x, aggregate(operation, xs, defaultValue))
  }

  def getBestAssumption(equations: HashSet[FunEqEquation]): Option[FunEqExpression] = {
          equations.flatMap(getAssumptions) match {
          case x if x.isEmpty => None
          case assumptions => Some(assumptions.groupBy(identity).maxBy(_._2.size)._1)
        }
      }

  private def getAssumptions(equation: FunEqEquation): HashSet[FunEqExpression] = equation match {
    case FunEqEquation(_, FunEqNode("*", _, _), FunEqIntLeaf(0), true)
    => getConstantFactors(equation.left)

    case FunEqEquation(_, left, right, true)
    => getConstantFactors(left) intersect getConstantFactors(right)

    case FunEqEquation(_, _, _, false) => HashSet()
  }

    private def getConstantFactors(expression: FunEqExpression): HashSet[FunEqExpression] = expression match {
      case FunEqNode("*", left, right) => getConstantFactors(left) ++ getConstantFactors(right)
      case FunEqFunc(_, argument) if constantsOnly(argument) => HashSet(expression)
      case _ => HashSet()
    }

    private def constantsOnly(expression: FunEqExpression): Boolean = Info.getAllVariables(expression).isEmpty
}
