package solver

import parser.{FunEqEquation, FunEqExpr, FunEqNode, FunEqFunc, FunEqIntLeaf, FunEqVarLeaf}
import java.util.UUID.randomUUID

object Substitutor {

  def sub(expr: FunEqEquation, variable: String, value: Int): FunEqEquation = {
    expr match {
      case FunEqEquation(g, _, left, right)
        => FunEqEquation(randomUUID.toString, List(g), sub(left, variable, value), sub(right, variable, value))
    }
  }

  def sub(expr: FunEqExpr, variable: String, value: Int): FunEqExpr = {
    expr match {
      case FunEqVarLeaf(`variable`) => FunEqIntLeaf(value)
      case FunEqVarLeaf(name) => FunEqVarLeaf(name)
      case FunEqNode(op, left, right) => FunEqNode(op, sub(left, variable, value), sub(right, variable, value))
      case FunEqFunc(name, argument) => FunEqFunc(name, sub(argument, variable, value))
      case FunEqIntLeaf(v) => FunEqIntLeaf(v)
    }
  }

  def allVariables(expr: FunEqEquation): Set[String] = {
    expr match {
      case FunEqEquation(_, _, left, right) => allVariables(left) ++ allVariables(right)
    }
  }

  def allVariables(expr: FunEqExpr): Set[String] = {
    expr match {
      case FunEqVarLeaf(v) => Set(v)
      case FunEqNode(_, left, right) => allVariables(left) ++ allVariables(right)
      case FunEqFunc(_, argument) => allVariables(argument)
      case FunEqIntLeaf(_) => Set()
    }
  }
}
