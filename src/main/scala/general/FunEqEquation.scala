package general

case class FunEqEquation(source: FunEqSource, left: FunEqExpression, right: FunEqExpression, isEquality: Boolean) {

  private def sign: String = if(isEquality) "=" else "!="

  def isTrivial: Boolean = {
    if (isEquality) left == right
    else (left, right) match {
      case (FunEqIntLeaf(a), FunEqIntLeaf(b)) if a != b => true
      case _ => false
    }
  }

  def print(detailed: Boolean): Unit =
    if (detailed) {
      println(s"$this [$source]")
    } else println(this)

  override def toString: String = s"${left.print(0)} $sign ${right.print(0)}"

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: FunEqEquation =>
        isEquality == other.isEquality &&
          (left == other.left && right == other.right) || (left == other.right && right == other.left)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val leftHashCode = left.hashCode
    val rightHashCode = right.hashCode

    if (leftHashCode < rightHashCode)
      leftHashCode * 19 - rightHashCode * 17 + isEquality.hashCode
    else
      rightHashCode * 19 - leftHashCode * 17 + isEquality.hashCode
  }
}
