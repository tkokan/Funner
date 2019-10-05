package processor.simplifier

import general._
import processor.PiecewiseProcessor

object ConstantsSimplifier extends PiecewiseProcessor {

  override val description: String = "Simplify constants."

  final override def process(expression: FunEqExpression): FunEqExpression =
    expression match {

      case FunEqFunc(name, argument) => FunEqFunc(name, process(argument))

      case FunEqNode(op, left, right) =>
        val simpleLeft = process(left)
        val simpleRight = process(right)

        (op, simpleLeft, simpleRight) match {
          case ("+", FunEqIntLeaf(a), FunEqIntLeaf(b)) => FunEqIntLeaf(a + b)
          case ("*", FunEqIntLeaf(a), FunEqIntLeaf(b)) => FunEqIntLeaf(a * b)
          case ("+", FunEqIntLeaf(0), x) => x

          // this won't be necessary for normalised equations ("smaller" side on the left side)
          case ("+", x, FunEqIntLeaf(0)) => x

          case ("*", FunEqIntLeaf(1), x) => x

          // also won't be necessary
          case ("*", x, FunEqIntLeaf(1)) => x

          case ("*", FunEqIntLeaf(0), _) => FunEqIntLeaf(0)

          // also won't be necessary
          case ("*", _, FunEqIntLeaf(0)) => FunEqIntLeaf(0)

          case _ => FunEqNode(op, simpleLeft, simpleRight)
        }

      case other => other
    }
}
