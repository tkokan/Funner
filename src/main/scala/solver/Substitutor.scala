package solver

import general.{FunEqEquation, FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode, FunEqSource, FunEqVarLeaf}


object Substitutor {

  def sub(equation: FunEqEquation, variable: String, value: Int): FunEqEquation = {
    equation match {
      case FunEqEquation(_, left, right)
        => FunEqEquation(
          FunEqSource(List(equation), s"Substitution: $variable = $value"),
          sub(left, variable, value),
          sub(right, variable, value))
    }
  }

  def sub(expr: FunEqExpression, variable: String, value: Int): FunEqExpression = expr match {
    case FunEqVarLeaf(`variable`) => FunEqIntLeaf(value)
    case FunEqVarLeaf(name) => FunEqVarLeaf(name)
    case FunEqNode(op, left, right) => FunEqNode(op, sub(left, variable, value), sub(right, variable, value))
    case FunEqFunc(name, argument) => FunEqFunc(name, sub(argument, variable, value))
    case FunEqIntLeaf(v) => FunEqIntLeaf(v)
  }

  def allVariables(equation: FunEqEquation): Set[String] = equation match {
    case FunEqEquation(_, left, right) => allVariables(left) ++ allVariables(right)
  }

  def allVariables(expr: FunEqExpression): Set[String] = expr match {
    case FunEqVarLeaf(v) => Set(v)
    case FunEqNode(_, left, right) => allVariables(left) ++ allVariables(right)
    case FunEqFunc(_, argument) => allVariables(argument)
    case FunEqIntLeaf(_) => Set()
  }
}
