package parser

sealed abstract class FunEqTreeExpr {
  def print(level: Int): String
}

case class FunEqTreeIntLeaf(value: Int) extends FunEqTreeExpr {
  def print(level: Int): String = value.toString
}

case class FunEqTreeVarLeaf(value: String) extends FunEqTreeExpr {
  def print(level: Int): String = value
}

case class FunEqTreeNode(op: String, left: FunEqTreeExpr, right: FunEqTreeExpr) extends FunEqTreeExpr {
  def print(level: Int): String = {
    val inner = left.print(level + 1) + " " + op + " " + right.print(level + 1)
    if (level == 0)
      inner
    else
      "(" + inner + ")"
  }
}

case class FunEqTreeFunc(op: String, operand: FunEqTreeExpr) extends FunEqTreeExpr {
  def print(level: Int): String = op + "(" + operand.print(level) + ")"
}

case class FunEqTreeEquation(left: FunEqTreeExpr, right: FunEqTreeExpr) {
  override def toString: String = left.print(0) + " = " + right.print(0)
}
