package processor.combinator

import general.{FunEqEquation, FunEqExpression, FunEqFunc, FunEqNode, FunEqSource}
import processor.Processor
import processor.simplifier.BigSimplifier

import scala.collection.immutable.HashSet

object EquationsSubsitutor extends Processor {
  override def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] = {

    val relevantEquations = equations
      .filter(!_.isTrivial)
      .filter(_.isEquality)

    for (
      into <- relevantEquations;
      from <- relevantEquations
    ) yield insert(into, from)
  }.flatten ++ equations

  def insert(into: FunEqEquation, from: FunEqEquation): HashSet[FunEqEquation] = {
    val source = FunEqSource(s"EquationsSubsitutor: $from -> $into", List(into, from))

    from match {
      case FunEqEquation(_, left, right, true) if left.complexity < right.complexity
      => HashSet(sub(source, into, right, left))

      case FunEqEquation(_, left, right, true) if left.complexity > right.complexity
      => HashSet(sub(source, into, left, right))

      case FunEqEquation(_, left, right, true)
      => HashSet(sub(source, into, left, right), sub(source, into, right, left))
    }
  }

  private def sub(src: FunEqSource, eq: FunEqEquation, oldExpr: FunEqExpression, newExpr: FunEqExpression): FunEqEquation = {
    eq match {
      case FunEqEquation(_, left, right, true)
      => val newEquation = new BigSimplifier().simplify(
        FunEqEquation(src, sub(left, oldExpr, newExpr), sub(right, oldExpr, newExpr), isEquality = true)
      )

      // make sure we don't override source unless something had changed
      if (newEquation == eq)
        eq
      else
        newEquation
    }
  }

  private def sub(expression: FunEqExpression, oldExpr: FunEqExpression, newExpr: FunEqExpression): FunEqExpression = {
    if (expression == oldExpr)
      newExpr
    else expression match {
      case FunEqFunc(name, argument) => FunEqFunc(name, sub(argument, oldExpr, newExpr))
      case FunEqNode(op, left, right) => FunEqNode(op, sub(left, oldExpr, newExpr), sub(right, oldExpr, newExpr))
      case _ => expression
    }
  }
}
