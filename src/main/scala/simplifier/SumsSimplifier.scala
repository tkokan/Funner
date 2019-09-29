package simplifier

import general.{FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode}

class SumsSimplifier extends AbstractSimplifier {
  override val description: String = "Sum to product."

  override def simplify(expression: FunEqExpression): FunEqExpression =
    expression match {

      case FunEqFunc(name, argument) => FunEqFunc(name, simplify(argument))

      case FunEqNode(op, left, right) =>
        val simpleLeft = simplify(left)
        val simpleRight = simplify(right)

        (op, simpleLeft, simpleRight) match {
          case ("+", a, b) if a == b => FunEqNode("*", FunEqIntLeaf(2), a)
          case _ => FunEqNode(op, simpleLeft, simpleRight)
        }

      case other => other
    }
}
