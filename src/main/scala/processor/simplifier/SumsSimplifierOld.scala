package processor.simplifier

import general.{FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode}
import processor.PiecewiseProcessor

object SumsSimplifierOld extends PiecewiseProcessor {

  override val description: String = "Sum to product."

  final override def process(expression: FunEqExpression): FunEqExpression =
    expression match {

      case FunEqFunc(name, argument) => FunEqFunc(name, process(argument))

      case FunEqNode(op, left, right) =>
        val simpleLeft = process(left)
        val simpleRight = process(right)

        (op, simpleLeft, simpleRight) match {
          case ("+", a, b) if a == b => FunEqNode("*", FunEqIntLeaf(2), a)
          case _ => FunEqNode(op, simpleLeft, simpleRight)
        }

      case other => other
    }
}
