package processor

import general.FunEqEquation

import scala.collection.immutable.HashSet

trait Processor {
  def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation]
}
