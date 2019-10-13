package processor.simplifier

import general.BinaryOperation.BinaryOperation
import general._
import processor.AuxProcessor.{aggregate, getOperands}

object SumsSimplifier extends AssociativeOperationSimplifier with Simplifier {

  override protected val operation: BinaryOperation = BinaryOperation.+
  override protected val neturalElement: Int = 0

  override def simplify(equation: FunEqEquation): FunEqEquation = {
    val summands = getSummands(equation)
    val modifiedSummands = summands.map(simplifyExpression)
    val processed = processOperands(modifiedSummands)

    val simplifiedEquation = createEquation(equation, processed)

    simplifiedEquation
  }

  private def createEquation(equation: FunEqEquation, expressions: List[FunEqExpression]): FunEqEquation = {

    val lhs = expressions.flatMap(x => leftRight(x)._1)
    val rhs = expressions.flatMap(x => leftRight(x)._2)

    (lhs, rhs) match {
      case (Nil, _) => createSimpleEquation(equation, rhs)
      case (_, Nil) => createSimpleEquation(equation, lhs)
      case _ =>
        val side1 = aggregate(operation, lhs)
        val side2 = aggregate(operation, rhs)

        if (side1.complexity > side2.complexity)
          FunEqEquation(equation.source, side1, side2, equation.isEquality)
        else
          FunEqEquation(equation.source, side2, side1, equation.isEquality)
    }
  }

  private def createSimpleEquation(equation: FunEqEquation, expressions: List[FunEqExpression]): FunEqEquation = {
    val (int, nonInt) = expressions.partition(isIntLeaf)

    val right = int match {
      case Nil => FunEqIntLeaf(0)
      case List(FunEqIntLeaf(v)) => FunEqIntLeaf(-v)
    }

    FunEqEquation(
      equation.source,
      aggregate(operation, nonInt),
      right,
      equation.isEquality
    )
  }

  private def isIntLeaf(expression: FunEqExpression): Boolean = {
    expression match {
      case FunEqIntLeaf(_) => true
      case _ => false
    }
  }

  // Negations, negative integers and expressions of the type a*x where
  // a is a negative integer go to the RHS, everything else goes to the LHS
  private def leftRight(expression: FunEqExpression): (Option[FunEqExpression], Option[FunEqExpression]) =
    expression match {
      case FunEqIntLeaf(a) if a < 0 => (None, Some(FunEqIntLeaf(-a)))

      case FunEqNode(BinaryOperation.*, FunEqIntLeaf(a), x) if a < 0 =>
        (None, Some(FunEqNode(BinaryOperation.*, FunEqIntLeaf(-a), x)))

      case FunEqNode(BinaryOperation.*, x, FunEqIntLeaf(a)) if a < 0 =>
        (None, Some(FunEqNode(BinaryOperation.*, FunEqIntLeaf(-a), x)))

      case _ => (Some(expression), None)
  }

  private def getSummands(equation: FunEqEquation): List[FunEqExpression] = {
      val leftSummands = getOperands(operation, equation.left)
      val rightSummands = getOperands(operation, equation.right)

      leftSummands ++ rightSummands.map(negate)
  }

  private def negate(expression: FunEqExpression): FunEqExpression = expression match {
    case FunEqIntLeaf(x) => FunEqIntLeaf(-x)
    case _ => FunEqNode(BinaryOperation.*, FunEqIntLeaf(-1), expression)
  }

  def combineTwo(x: FunEqExpression, y: FunEqExpression): Option[FunEqExpression] = {
    (x, y) match {
      case (FunEqIntLeaf(a), FunEqIntLeaf(b)) => Some(FunEqIntLeaf(a + b))
      case _ =>
        val xFactors = getOperands(BinaryOperation.*, x)
        val yFactors = getOperands(BinaryOperation.*, y)

        val commonFactors = xFactors intersect yFactors

        commonFactors match {
          case Nil => None
          case _ =>
            val xOther = xFactors.diff(commonFactors)
            val yOther = yFactors.diff(commonFactors)

            val separateParts = FunEqNode(
              operation,
              aggregate(BinaryOperation.*, xOther),
              aggregate(BinaryOperation.*, yOther)
            )

            Some(aggregate(BinaryOperation.*, commonFactors :+ separateParts))
        }
    }
  }
}
