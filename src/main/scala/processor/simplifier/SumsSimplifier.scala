package processor.simplifier

import general.{FunEqEquation, FunEqExpression, FunEqIntLeaf, FunEqNode, FunEqSource}
import processor.AuxProcessor.{aggregate, getOperands}

object SumsSimplifier extends AssociativeOperationSimplifier with Simplifier {

  override protected val operation: String = "+"
  override protected val neturalElement: Int = 0

  override def simplify(equation: FunEqEquation): FunEqEquation = {
    val summands = getSummands(equation)
    val modifiedSummands = summands.map(simplifyExpression)
    val processed = processOperands(modifiedSummands)

    val simplifiedEquation = createEquation(equation, processed)

//    println(s"$equation [${equation.source}] -> $simplifiedEquation")

    simplifiedEquation
  }

  private def createEquation(equation: FunEqEquation, expressions: List[FunEqExpression]): FunEqEquation = {

    val lhs = expressions.flatMap(x => leftRight(x)._1)
    val rhs = expressions.flatMap(x => leftRight(x)._2)

    (lhs, rhs) match {
      case (Nil, _) => createSimpleEquation(equation, rhs)
      case (_, Nil) => createSimpleEquation(equation, lhs)
      case _ =>
        val side1 = aggregate(operation, lhs, 0)
        val side2 = aggregate(operation, rhs, 0)

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
      aggregate(operation, nonInt, 0),
      right,
      equation.isEquality
    )
  }

  private def isIntLeaf(expression: FunEqExpression): Boolean = expression match {
    case FunEqIntLeaf(_) => true
    case _ => false
  }

  // Negations, negative integers and expressions of the type a*x where
  // a is a negative integer go to the RHS, everything else goes to the LHS
  private def leftRight(expression: FunEqExpression): (Option[FunEqExpression], Option[FunEqExpression]) =
    expression match {
      // case FunEqNegation(x) => (None, Some(x))
      case FunEqIntLeaf(a) if a < 0 => (None, Some(FunEqIntLeaf(-a)))
      case FunEqNode("*", FunEqIntLeaf(a), x) if a < 0 => (None, Some(FunEqNode("*", FunEqIntLeaf(-a), x)))
      case FunEqNode("*", x, FunEqIntLeaf(a)) if a < 0 => (None, Some(FunEqNode("*", FunEqIntLeaf(-a), x)))
      case _ => (Some(expression), None)
  }

  private def getSummands(equation: FunEqEquation): List[FunEqExpression] = {
      val leftSummands = getOperands(operation, equation.left)
      val rightSummands = getOperands(operation, equation.right)

      (leftSummands ++ rightSummands.map(negate))
  }

  private def negate(expression: FunEqExpression): FunEqExpression = expression match {
    case FunEqIntLeaf(x) => FunEqIntLeaf(-x)
    case _ => FunEqNode("*", FunEqIntLeaf(-1), expression)
  }

//  private def extractNegations(expression: FunEqExpression): FunEqExpression = {
//    val factors = getOperands("*", expression)
//
//    val negations = factors.map(countNegations)
//
//    val numNegations = negations.map(_._2).sum
//    val simplifiedFactors = negations.map(_._1).partition(x => x.isInstanceOf[FunEqIntLeaf])
//
//    val integerPart = simplifiedFactors._1.map({case FunEqIntLeaf(a) => a}).product
//    val integerPartWithNegation = if (numNegations % 2 == 0) integerPart else -integerPart
//
//    val nonIntegerPart = simplifiedFactors._2
//
//    integerPartWithNegation match {
//      case 0 => FunEqIntLeaf(0)
//      case 1 => aggregate("*", nonIntegerPart, 1)
//      case -1 => FunEqNegation(aggregate("*", nonIntegerPart, 1))
//      case a if a > 1 => aggregate("*", FunEqIntLeaf(a) :: nonIntegerPart, 1)
//      case a if a < -1 => FunEqNegation(aggregate("*", FunEqIntLeaf(-a) :: nonIntegerPart, 1))
//    }
//  }

//  private def countNegations(expression: FunEqExpression): (FunEqExpression, Int) =
//    countNegations(expression, 0)

//  @tailrec
//  private def countNegations(expression: FunEqExpression, count: Int): (FunEqExpression, Int) = {
//    expression match {
//      case FunEqNegation(x) => countNegations(x, count + 1)
//      case _ => (expression, count)
//    }
//  }

  def combineTwo(x: FunEqExpression, y: FunEqExpression): Option[FunEqExpression] = (x, y) match {
    case (FunEqIntLeaf(a), FunEqIntLeaf(b)) => Some(FunEqIntLeaf(a + b))
//    case (FunEqNegation(FunEqIntLeaf(a)), _) => combineTwo(FunEqIntLeaf(-a), y)
//    case (_, FunEqNegation(FunEqIntLeaf(b))) => combineTwo(x, FunEqIntLeaf(-b))
    case _ =>
      val xFactors = getOperands("*", x)
      val yFactors = getOperands("*", y)

      val commonFactors = xFactors intersect yFactors

      commonFactors match {
        case Nil => None
        case _ => {
          val xOther = xFactors.diff(commonFactors)
          val yOther = yFactors.diff(commonFactors)

          val separateParts = FunEqNode(
            operation,
            aggregate("*", xOther, 1),
            aggregate("*", yOther, 1)
          )

          Some(aggregate("*", commonFactors :+ separateParts, 1))
        }
      }
  }
}
