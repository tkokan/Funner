package solver

import parser.FunEqParser

import scala.collection.immutable.HashSet

object Tester extends App {

  test()

  def test(): Unit = {
    val input = "f(x) * f(y) = f(x + y) + x*y"

    println(s"Input: $input")

    val parser = new FunEqParser()
    val equation = parser.parseEquation(input).get

    println(s"Equation: $equation")
    println()

    Solver.solve(equation, detailedPrint = true)
  }
}