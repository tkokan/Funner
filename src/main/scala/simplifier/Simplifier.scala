package simplifier

import general.FunEqEquation

object Simplifier {

  private val simplifiers = List(
    new AdditionSimplifier,
    new ConstantsSimplifier,
    new SumsSimplifier,
    new VariablesNormaliser,
    new MultiplicationSimplifier
  )

  private val composedSimplifier = simplifiers
    .map(x => (y: FunEqEquation) => x.simplify(y))
    .foldLeft((x: FunEqEquation) => x)(_ compose _)

  def simplify(equation: FunEqEquation): FunEqEquation = {
    val simplified = composedSimplifier(equation)

    if (simplified == equation)
      simplified
    else
      simplify(simplified)
  }
}
