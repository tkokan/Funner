package simplifier

import general.{FunEqEquation, FunEqIntLeaf, FunEqNode, FunEqSource}

class MultiplicationSimplifier extends AbstractSimplifier {

  override val description: String = "Cancel multiplication."

  override def simplify(equation: FunEqEquation): FunEqEquation = {
    val source = FunEqSource(List(equation), description)

    equation match {
      case FunEqEquation(_, FunEqNode("*", FunEqIntLeaf(a), x), FunEqNode("*", FunEqIntLeaf(b), y)) if a == b && a != 0
      => FunEqEquation(source, x, y)

      case FunEqEquation(_, FunEqNode("*", FunEqIntLeaf(a), x), FunEqNode("*", y, FunEqIntLeaf(b))) if a == b && a != 0
      => FunEqEquation(source, x, y)

      case FunEqEquation(_, FunEqNode("*", x, FunEqIntLeaf(a)), FunEqNode("*", FunEqIntLeaf(b), y)) if a == b && a != 0
      => FunEqEquation(source, x, y)

      case FunEqEquation(_, FunEqNode("*", x, FunEqIntLeaf(a)), FunEqNode("*", y, FunEqIntLeaf(b))) if a == b && a != 0
      => FunEqEquation(source, x, y)

      case other => other
    }
  }
}
