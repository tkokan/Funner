package processor

import general.FunEqEquation

import scala.collection.immutable.HashSet

abstract class IndependentProcessor extends Processor {

  final override def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] =
    equations.flatMap(process).filter(x => !x.isTrivial)

  def process(equation: FunEqEquation): HashSet[FunEqEquation]
}
