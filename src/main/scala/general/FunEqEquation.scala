package general

case class FunEqEquation(source: FunEqSource, left: FunEqExpression, right: FunEqExpression) {

  def isTrivial: Boolean = left == right

  override def toString: String = left.print(0) + " = " + right.print(0)

  override def equals(obj: Any): Boolean = obj match {
    case other: FunEqEquation =>
      (left == other.left && right == other.right) || (left == other.right && right == other.left)
    case _ => false
  }

  override def hashCode(): Int = {
    val leftHashCode = left.hashCode
    val rightHashCode = right.hashCode

    if (leftHashCode < rightHashCode)
      leftHashCode * 19 - rightHashCode * 17
    else
      rightHashCode * 19 - leftHashCode * 17
  }
}
