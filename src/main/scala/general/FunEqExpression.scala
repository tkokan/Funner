package general

import general.BinaryOperation.BinaryOperation

sealed abstract class FunEqExpression {
  def print(level: Int): String

  def complexity: Int

  override def toString: String = print(0)
}

case class FunEqIntLeaf(value: Int) extends FunEqExpression {
  def print(level: Int): String = if (value >= 0 || level == 0) value.toString else s"($value)"

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: FunEqIntLeaf => value == other.value
      case _ => false
    }
  }

  override def complexity: Int = 1
}

case class FunEqVarLeaf(name: String) extends FunEqExpression {
  def print(level: Int): String = name

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: FunEqVarLeaf => name == other.name
      case _ => false
    }
  }

  override def complexity: Int = 1
}

case class FunEqNode(
  op: BinaryOperation,
  left: FunEqExpression,
  right: FunEqExpression) extends FunEqExpression {

  def print(level: Int): String = {
    val inner = s"${left.print(level + 1)} $op ${right.print(level + 1)}"
    if (level == 0)
      inner
    else
      s"($inner)"
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: FunEqNode
      => ((op == other.op)
        && ((left == other.left && right == other.right) || (right == other.left && left == other.right)))
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val leftHashCode = left.hashCode
    val rightHashCode = right.hashCode

    if (leftHashCode < rightHashCode)
      leftHashCode * 19 - rightHashCode * 17 + 23 * op.hashCode
    else
      rightHashCode * 19 - leftHashCode * 17 + 23 * op.hashCode
  }

  override def complexity: Int = left.complexity + right.complexity + 1
}

case class FunEqFunc(name: String, argument: FunEqExpression) extends FunEqExpression {
  def print(level: Int): String = s"$name(${argument.print(0)})"

  override def equals(obj: Any): Boolean = obj match {
    case other: FunEqFunc => (name == other.name) && (argument == other.argument)
    case _ => false
  }

  override def complexity: Int = argument.complexity + 1
}
