package processor.combinator

import general.{BinaryOperation, FunEqEquation, FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode, FunEqSource, FunEqVarLeaf}
import processor.{AuxProcessor, IndependentProcessor}

import scala.collection.immutable.HashSet

object VariablesShifter extends IndependentProcessor {
  override def process(equation: FunEqEquation): HashSet[FunEqEquation] = {
    HashSet(equation) ++ getLinearArguments(equation).map(arg => shiftVariable(equation, arg))
  }

  private def getLinearArguments(equation: FunEqEquation): HashSet[(FunEqVarLeaf, FunEqIntLeaf)] = {
    getLinearArguments(equation.left) ++ getLinearArguments(equation.right)
  }

  private def getLinearArguments(expression: FunEqExpression): HashSet[(FunEqVarLeaf, FunEqIntLeaf)] = {
    expression match {
      case FunEqFunc(_, argument) => {
        extractLinear(argument) match {
          case Some(x) => HashSet(x)
          case None => getLinearArguments(argument)
        }
      }
      case FunEqNode(_, left, right) => getLinearArguments(left) ++ getLinearArguments(right)
      case _ => HashSet()
    }
  }

  private def shiftVariable(equation: FunEqEquation, linearArgument: (FunEqVarLeaf, FunEqIntLeaf)): FunEqEquation = {
    val (variable, constant) = linearArgument
    val shiftExpression = FunEqNode(BinaryOperation.+, variable, FunEqIntLeaf(-constant.value))
    val source = FunEqSource(s"Shift [$variable <- $shiftExpression] :: $equation", List(equation))

    AuxProcessor.substitute(equation, variable, shiftExpression, source)
  }

  private def extractLinear(expression: FunEqExpression): Option[(FunEqVarLeaf, FunEqIntLeaf)] = {
    expression match {
      case FunEqNode(BinaryOperation.+, FunEqVarLeaf(v), FunEqIntLeaf(c)) =>
        Some(FunEqVarLeaf(v), FunEqIntLeaf(c))
      case FunEqNode(BinaryOperation.+, FunEqIntLeaf(c), FunEqVarLeaf(v)) =>
        Some(FunEqVarLeaf(v), FunEqIntLeaf(c))
      case _ => None
    }
  }
}
