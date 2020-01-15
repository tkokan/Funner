package processor.combinator

import general._
import processor.{AuxProcessor, Processor}

import scala.collection.immutable.HashSet

object EquationsSubsitutor extends Processor {
  override def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] = {
    val relevantEquations = equations
      .filter(!_.isTrivial)
      .filter(_.isEquality)

    val newEqs = {
      for (
        from <- relevantEquations;
        into <- relevantEquations
      ) yield insert(from, into)
    }.flatten

    newEqs ++ equations
  }

  def insert(from: FunEqEquation, into: FunEqEquation): HashSet[FunEqEquation] = {
    val possibleNewSource = FunEqSource(s"EquationsSubsitutor: $from -> $into", List(from, into))

    from match {
      case FunEqEquation(_, left, right, true) if left.complexity < right.complexity =>
        HashSet(AuxProcessor.substitute(into, right, left, possibleNewSource))

      case FunEqEquation(_, left, right, true) if left.complexity > right.complexity =>
        HashSet(AuxProcessor.substitute(into, left, right, possibleNewSource))

      case FunEqEquation(_, left, right, true) =>
        HashSet(
          AuxProcessor.substitute(into, left, right, possibleNewSource),
          AuxProcessor.substitute(into, right, left, possibleNewSource))
    }
  }
}
