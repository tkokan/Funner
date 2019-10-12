package processor.remover

import general.{FunEqEquation, FunEqFunc, FunEqIntLeaf}
import processor.Processor
import processor.combinator.EquationsSubsitutor

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Remover extends Processor {
  override def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] = {

    val (functionValues, otherEquations) = equations.partition(isFunctionValue)

    val processedEquations = otherEquations
      .map(x => insertAll(functionValues.toList, x))
      .filter(!_.isTrivial)

    functionValues ++ processedEquations
  }

  // This assumes normal form
  private def isFunctionValue(equation: FunEqEquation): Boolean = equation match {
    case FunEqEquation(_, FunEqFunc(_, FunEqIntLeaf(_)), FunEqIntLeaf(_), true) => true
    case _ => false
  }

  @tailrec
  private def insertAll(functionValues: List[FunEqEquation], equation: FunEqEquation): FunEqEquation =
    if(equation.isEquality) {
      functionValues match {
        case Nil => equation
        case v :: vs => insertAll(vs, insertSingle(into = equation, from = v))
      }
    }
    else
      equation

  private def insertSingle(into: FunEqEquation, from: FunEqEquation): FunEqEquation = {
    val multi = EquationsSubsitutor.insert(into, from).toList

    multi match {
      case List(a) => a
    }
  }
}
