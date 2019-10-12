package processor.simplifier

import general.FunEqEquation

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import processor.Processor

class BigSimplifier(assumptions: List[FunEqEquation] = List()) extends Simplifier with Processor {

  private val simplifiers: List[Simplifier] = List(
    SumsSimplifier,
    MultiplicationSimplifier,
    new CancelMultiplicationSimplifier(assumptions),
    VariablesNormaliser)

  @tailrec
  final override def simplify(equation: FunEqEquation): FunEqEquation = {

    val processed = composed(equation)

    if (processed == equation)
      processed
    else
      simplify(processed)
  }

  def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] = {
//    println(s"BigSimplifier :: ${equations.size} equations")
    equations.map(simplify)
  }

  private lazy val composed: FunEqEquation => FunEqEquation =
    simplifiers
      .reverse
      .map(x => (eq: FunEqEquation) => x.simplify(eq))
      .foldLeft(identity[FunEqEquation] _)(_ compose _)
}
