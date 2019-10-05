package processor

import general.FunEqEquation

import scala.collection.immutable.HashSet

abstract class SingleResultProcessor extends IndependentProcessor {
  final override def process(equation: FunEqEquation): HashSet[FunEqEquation] = HashSet(processOneResult(equation))

  def processOneResult(eqEquation: FunEqEquation): FunEqEquation
}
