package processor.simplifier

import general._
import processor.SingleResultProcessor
import scala.annotation.tailrec

object VariablesNormaliser extends SingleResultProcessor {

  private val variables = List("x", "y", "z", "w")

  @tailrec
  final override def processOneResult(equation: FunEqEquation): FunEqEquation = {
    val allVariables = Info.getAllVariables(equation)

    val firstPair = allPairs
      .find(p => !allVariables.contains(p._1) && allVariables.contains(p._2))

    firstPair match {
      case Some((first, second)) => processOneResult(sub(equation, oldName = second, newName = first))
      case None => equation
    }
  }

  private def allPairs: List[(String, String)] = {
    for {
      (x, idxX) <- variables.zipWithIndex
      (y, idxY) <- variables.zipWithIndex
      if idxX < idxY
    } yield (x, y)
  }

  private def sub(equation: FunEqEquation, oldName: String, newName: String): FunEqEquation = {
    equation match {
      case FunEqEquation(_, left, right)
      => FunEqEquation(
        FunEqSource(List(equation), s"Rename variable: $oldName -> $newName"),
        sub(left, oldName, newName),
        sub(right, oldName, newName))
    }
  }

  private def sub(expression: FunEqExpression, oldName: String, newName: String): FunEqExpression =
    expression match {
      case FunEqVarLeaf(`oldName`) => FunEqVarLeaf(newName)
      case FunEqNode(op, left, right) => FunEqNode(op, sub(left, oldName, newName), sub(right, oldName, newName))
      case FunEqFunc(name, argument) => FunEqFunc(name, sub(argument, oldName, newName))
      case other => other
    }
}
