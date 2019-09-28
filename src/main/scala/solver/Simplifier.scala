package solver

import parser.{FunEqEquation, FunEqExpr, FunEqNode, FunEqFunc, FunEqIntLeaf}
import java.util.UUID.randomUUID

object Simplifier {

  def simplify(equation: FunEqEquation): FunEqEquation = simplifyAddition(simplifyConstants(equation))

  private def simplifyAddition(equation: FunEqEquation): FunEqEquation = {
    equation match {
      case FunEqEquation(g, _, FunEqNode("+", l1, r1), FunEqNode("+", l2, r2)) if l1 == l2
        => FunEqEquation(randomUUID.toString, List(g), r1, r2)

      case FunEqEquation(g, _, FunEqNode("+", l1, r1), FunEqNode("+", l2, r2)) if r1 == r2
        => FunEqEquation(randomUUID.toString, List(g), l1, l2)

      case FunEqEquation(g, _, FunEqNode("+", l1, r1), FunEqNode("+", l2, r2)) if l1 == r2
        => FunEqEquation(randomUUID.toString, List(g), r1, l2)

      case FunEqEquation(g, _, FunEqNode("+", l1, r1), FunEqNode("+", l2, r2)) if l2 == r1
        => FunEqEquation(randomUUID.toString, List(g), r2, l1)

      case x => x
    }
  }

  private def simplifyConstants(equation: FunEqEquation): FunEqEquation = {
    equation match {
      case FunEqEquation(g, _, left, right)
        => FunEqEquation(randomUUID.toString, List(g), simplifyConstants(left), simplifyConstants(right))
    }
  }

  private def simplifyConstants(expr: FunEqExpr): FunEqExpr = {

    expr match {

      case FunEqFunc(name, argument) => FunEqFunc(name, simplifyConstants(argument))

      case FunEqNode(op, left, right) => {
        val simple_left = simplifyConstants(left)
        val simple_right = simplifyConstants(right)

        (op, simple_left, simple_right) match {
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

          case _ => FunEqNode(op, simple_left, simple_right)
        }
      }

      case y => y
    }
  }
}
