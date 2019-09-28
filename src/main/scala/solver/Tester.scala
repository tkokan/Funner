package solver

import parser.FunEqParser

object Tester extends App {

    val input = "2*f(x+y) = f(3*x) + f(3*y)"

    println("input: " + input)

    val parser = new FunEqParser()

    val equation = parser.parseEquation(input).get

    println("equation: " + equation)
    println("")

    val allEquations = Solver.expand(equation)

    //for(equation <- allEquations.toList.sortWith(_.toString.length < _.toString.length)) println(equation)

    println("***********")

    //Solver.printTrace(equation, "0 = f(x) + f(2)")
}