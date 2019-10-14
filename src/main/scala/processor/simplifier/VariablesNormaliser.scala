package processor.simplifier

import general._
import processor.AuxProcessor

import scala.annotation.tailrec

object VariablesNormaliser extends Simplifier {

  private val variables = List("x", "y", "z", "w")

  @tailrec
  final override def simplify(equation: FunEqEquation): FunEqEquation = {
    val allVariables = AuxProcessor.getAllVariables(equation)

    val firstPair = allPairs
      .find(p => !allVariables.contains(p._1) && allVariables.contains(p._2))

    firstPair match {
      case Some((first, second)) => simplify(sub(equation, oldName = second, newName = first))
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
      case FunEqEquation(_, left, right, isEquality)
      => FunEqEquation(
        FunEqSource(s"Rename variable: [$oldName -> $newName] :: $equation", List(equation)),
        sub(left, oldName, newName),
        sub(right, oldName, newName),
        isEquality
      )
    }
  }

  private def sub(expression: FunEqExpression, oldName: String, newName: String): FunEqExpression = {
    expression match {
      case FunEqVarLeaf(`oldName`) => FunEqVarLeaf(newName)
      case FunEqNode(op, left, right) => FunEqNode(op, sub(left, oldName, newName), sub(right, oldName, newName))
      case FunEqFunc(name, argument) => FunEqFunc(name, sub(argument, oldName, newName))
      case other => other
    }
  }
}
