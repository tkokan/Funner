package parser

sealed abstract class FunEqTreeExpr

case class FunEqTreeIntLeaf(value: Int) extends FunEqTreeExpr

case class FunEqTreeVarLeaf(value: String) extends FunEqTreeExpr

case class FunEqTreeNode(op: String, left: FunEqTreeExpr, right: FunEqTreeExpr) extends FunEqTreeExpr

case class FunEqTreeFunc(op: String, operand: FunEqTreeExpr) extends FunEqTreeExpr

case class FunEqTreeEquation(left: FunEqTreeExpr, right: FunEqTreeExpr)
