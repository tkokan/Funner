package general

import scala.collection.immutable.HashSet

object Info {

  def getAllVariables(equation: FunEqEquation): HashSet[String] = {
    equation match {
      case FunEqEquation(_, left, right, _) => getAllVariables(left) ++ getAllVariables(right)
    }
  }

  def getAllVariables(expr: FunEqExpression): HashSet[String] = {
    expr match {
      case FunEqVarLeaf(v) => HashSet(v)
      case FunEqNode(_, left, right) => getAllVariables(left) ++ getAllVariables(right)
      case FunEqFunc(_, argument) => getAllVariables(argument)
      case FunEqIntLeaf(_) => HashSet()
    }
  }
}
