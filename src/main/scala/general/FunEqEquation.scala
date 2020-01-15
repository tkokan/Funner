package general

import processor.AuxProcessor

import scala.collection.immutable.HashSet

case class FunEqEquation(source: FunEqSource, left: FunEqExpression, right: FunEqExpression, isEquality: Boolean) {

  private def sign: String = if(isEquality) "=" else "!="

  def isTrivial: Boolean = {
    (left, right, isEquality) match {
      case (l, r, true) => l == r
      case (FunEqIntLeaf(a), FunEqIntLeaf(b), false) if a != b => true
      case _ => false
    }
  }

  def isImpossible: Boolean = {
    (left, right, isEquality) match {
      case (FunEqIntLeaf(a), FunEqIntLeaf(b), true) if a != b => true
      case (FunEqIntLeaf(a), FunEqIntLeaf(b), false) if a == b => true
      case _ => false
    }
  }

  def print(detailed: Boolean, prefix: String = ""): Unit =
    if (detailed) {
      println(s"$prefix$this [$source]")
    } else println(s"$prefix$this")

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

  def isSolution: Boolean = {
    this match {
      case FunEqEquation(_, FunEqFunc(_, FunEqVarLeaf("x")), right, true) if AuxProcessor.xOnly(right) => true
      case _ => false
    }
  }
}
