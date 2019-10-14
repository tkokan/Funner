package processor.combinator

import general._
import processor.{AuxProcessor, IndependentProcessor}

import scala.collection.immutable.HashSet

object ConstantsSubstitutor extends IndependentProcessor {

  override def process(equation: FunEqEquation): HashSet[FunEqEquation] = {
    val newEquations = {
        for (
          variable <- AuxProcessor.getAllVariables(equation);
          value <- -1 to 2
        ) yield sub(equation, variable, value)
    }

    newEquations + equation
  }

  private def sub(equation: FunEqEquation, variable: String, value: Int): FunEqEquation = {
    val possibleNewSouce = FunEqSource(s"Substitution [$variable <- $value] in $equation", List(equation))
    AuxProcessor.substitute(equation, FunEqVarLeaf(variable), FunEqIntLeaf(value), possibleNewSouce)
  }
}
