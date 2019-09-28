package parser

sealed abstract class FunEqExpr {
  def print(level: Int): String

  override def toString: String = print(0)
}

case class FunEqIntLeaf(value: Int) extends FunEqExpr {
  def print(level: Int): String = if (value >= 0 | level == 0) value.toString else "(" + value.toString + ")"

  def ==(other: FunEqExpr): Boolean = {
    other match {
      case FunEqIntLeaf(v) => this.value == v
      case _ => false
    }
  }
}

case class FunEqVarLeaf(name: String) extends FunEqExpr {
  def print(level: Int): String = name

  def ==(other: FunEqExpr): Boolean = {
    other match {
      case FunEqVarLeaf(n) => this.name == n
      case _ => false
    }
  }
}

case class FunEqNode(op: String, left: FunEqExpr, right: FunEqExpr) extends FunEqExpr {
  def print(level: Int): String = {
    val inner = left.print(level + 1) + " " + op + " " + right.print(level + 1)
    if (level == 0)
      inner
    else
      "(" + inner + ")"
  }

  def ==(other: FunEqExpr): Boolean = {
    other match {
      case FunEqNode(o, a, b)
        => (this.op == o) & ((a == this.left & b == this.right) | (a == this.right & b == this.left))
      case _ => false
    }
  }
}

case class FunEqFunc(name: String, argument: FunEqExpr) extends FunEqExpr {
  def print(level: Int): String = name + "(" + argument.print(0) + ")"

  def ==(other: FunEqExpr): Boolean = {
    other match {
      case FunEqFunc(_, a) => this.argument == a
      case _ => false
    }
  }
}

case class FunEqEquation(guid: String, parents: List[String], left: FunEqExpr, right: FunEqExpr) {

  def isTautology: Boolean = left == right

  override def toString: String = left.print(0) + " = " + right.print(0)

  def ==(other: FunEqEquation): Boolean =
    (left == other.left & right == other.right) | (left == other.right & right == other.left)
}
