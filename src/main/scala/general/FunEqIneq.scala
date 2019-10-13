package general

case class FunEqIneq(source: FunEqSource, left: FunEqExpression, right: FunEqExpression) {

  def isTrivial: Boolean = {
    (left, right) match {
      case (FunEqIntLeaf(a), FunEqIntLeaf(b)) if a != b => true
      case _ => false
    }
  }

  def print(detailed: Boolean): Unit =
    if (detailed) println(s"$this [$source]")
    else println(this)

  override def toString: String = s"${left.print(0)} != ${right.print(0)}"

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: FunEqIneq =>
        (left == other.left && right == other.right) || (left == other.right && right == other.left)
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val leftHashCode = left.hashCode
    val rightHashCode = right.hashCode

    if (leftHashCode < rightHashCode)
      leftHashCode * 23 - rightHashCode * 17
    else
      rightHashCode * 23 - leftHashCode * 17
  }
}
