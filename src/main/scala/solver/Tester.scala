package solver

import parser.FunEqParser
import scala.collection.immutable.HashSet

object Tester extends App {

  test1()

  def test1(): Unit = {
    val input = "2*f(x+y) = f(3*x) + f(3*y)"

    println("input: " + input)

    val parser = new FunEqParser()

    val equation = parser.parseEquation(input).get

    println("equation: " + equation)
    println()

    val allEquations = Solver.expand(equation)

    println("Total equations: " + allEquations.size)
    println()

    for(equation <- allEquations.toList.sortWith(_.toString.length < _.toString.length)) println(equation)
  }

  def test2(): Unit = {
    val input1 = "2 * f(0) = f(3) + f(-3)"
    val input2 = "2 * f(0) = f(-3) + f(3)"

    val parser = new FunEqParser()

    val equation1 = parser.parseEquation(input1).get
    val equation2 = parser.parseEquation(input2).get

    if (equation1 == equation2)
      println("Same")
    else
      println("Different")

    val set = HashSet(equation1, equation2)

    println(equation1.hashCode())

    println(equation2.hashCode())

    println(set.size)
  }
}