package processor.simplifier

import general.FunEqEquation

trait Simplifier {
  def simplify(equation: FunEqEquation) : FunEqEquation
}
