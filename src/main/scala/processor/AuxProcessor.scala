package processor

import general.BinaryOperation.BinaryOperation
import general._
import processor.simplifier.BigSimplifier

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/**
 * Performs some general equation and expression manipulations.
 */
object AuxProcessor {

  /**
   * Extract summands or factors from the expression.
   * For example 5 + (x + 2*y) -> List(5, x, 2*y), when operation is '+'
   * or (7 * x) * (f(x)+1) -> List(7, x, f(x)+1), when operation is '*'.
   *
   * @param operation - binary operation, either '+' or '*'.
   * @param expression where to extract operands from
   * @return a list of summands or factors
   */
  def getOperands(
    operation: BinaryOperation,
    expression: FunEqExpression): List[FunEqExpression] =
      getOperandsAcc(operation, List(expression), List())

  @tailrec
  private def getOperandsAcc(
    operation: BinaryOperation,
    expressions: List[FunEqExpression],
    acc: List[FunEqExpression]): List[FunEqExpression] = {

    expressions match {
      //nothing left to process, we're done
      case Nil => acc

      // something to process
      case e :: es => e match {

        // e still needs to be processed, add both children to the rest of expressions
        case FunEqNode(op, left, right) if op == operation
        => getOperandsAcc(operation, es :+ left :+ right, acc)

        // e is done, add it to results, process the rest of expressions
        case _ => getOperandsAcc(operation, es, acc :+ e)
      }
    }
  }

  /**
   * Aggregate a list of operands. This is the inverse of the method getOperands above.
   * For example, List(1, 2, 3) -> 1 * (2 * 3) when operation is '*'.
   *
   * @param operation - either '+' or '*'
   * @param operands - a list of expressions to accumulate
   * @return accumulated expression
   */
  def aggregate(operation: BinaryOperation, operands: List[FunEqExpression]): FunEqExpression = {
    operands match {
        case Nil => FunEqIntLeaf(BinaryOperation.neutralElement(operation))
        case _ => operands.reduceRight(FunEqNode(operation, _, _))
    }
  }

  def getBestAssumption(equations: HashSet[FunEqEquation]): Option[FunEqExpression] = {
    equations.flatMap(getAssumptions) match {
      case x if x.isEmpty => None
      case assumptions => Some(assumptions.groupBy(identity).maxBy(_._2.size)._1)
    }
  }

  private def getAssumptions(equation: FunEqEquation): HashSet[FunEqExpression] = equation match {
    case FunEqEquation(_, FunEqNode(BinaryOperation.*, _, _), FunEqIntLeaf(0), true) =>
      getConstantFactors(equation.left)

    case FunEqEquation(_, left, right, true) =>
      getConstantFactors(left) intersect getConstantFactors(right)

    case FunEqEquation(_, _, _, false) => HashSet()
  }

  private def getConstantFactors(expression: FunEqExpression): HashSet[FunEqExpression] =
    HashSet() ++ getOperands(BinaryOperation.*, expression).filter(constantsOnly)

  private def constantsOnly(expression: FunEqExpression): Boolean =
    AuxProcessor.getAllVariables(expression).isEmpty

  def substitute(
    equation: FunEqEquation,
    from: FunEqExpression,
    to: FunEqExpression,
    possibleNewSource: FunEqSource): FunEqEquation = {

    val newEquation = FunEqEquation(
      possibleNewSource,
      substitute(equation.left, from, to),
      substitute(equation.right, from, to),
      equation.isEquality)

    val simplifiedNew = new BigSimplifier().simplify(newEquation)

    // make sure we don't override the source if nothing had changed
    if (simplifiedNew == equation) equation
    else simplifiedNew
  }

  def substitute(expression: FunEqExpression, from: FunEqExpression, to: FunEqExpression): FunEqExpression = {
    expression match {
      case e if e == from => to
      case FunEqFunc(name, e) => FunEqFunc(name, substitute(e, from, to))
      case FunEqNode(op, left, right) => FunEqNode(op, substitute(left, from, to), substitute(right, from, to))
      case _ => expression
    }
  }

  def getAllVariables(equation: FunEqEquation): HashSet[String] = {
    equation match {
      case FunEqEquation(_, left, right, _) => getAllVariables(left) ++ getAllVariables(right)
    }
  }

  def getAllVariables(expr: FunEqExpression): HashSet[String] = {
    expr match {
      case FunEqVarLeaf(v) => HashSet(v)
      case FunEqNode(_, left, right) => getAllVariables(left) ++ getAllVariables(right)
      case FunEqFunc(_, argument) => getAllVariables(argument)
      case FunEqIntLeaf(_) => HashSet()
    }
  }

  def xOnly(expression: FunEqExpression): Boolean =
    (AuxProcessor.getAllVariables(expression) diff HashSet("x")).isEmpty && !hasFunctionCalls(expression)

  def hasFunctionCalls(expression: FunEqExpression): Boolean = {
    expression match {
      case FunEqFunc(_, _) => true
      case FunEqNode(_, left, right) => hasFunctionCalls(left) || hasFunctionCalls(right)
      case _ => false
    }
  }
}
