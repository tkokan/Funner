package processor.simplifier

import general._
import processor.IndependentProcessor

import scala.collection.immutable.HashSet

object MultiplicationSimplifier extends IndependentProcessor {

  final override def process(equation: FunEqEquation): HashSet[FunEqEquation] = {
    val source = FunEqSource(List(equation), "Cancel multiplication.")

    HashSet(
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
    )
  }
}
