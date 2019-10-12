import general.{FunEqEquation, FunEqFunc, FunEqIntLeaf, FunEqNode, FunEqSource, FunEqVarLeaf}
import org.scalatest.FunSuite
import processor.simplifier.BigSimplifier

import scala.collection.immutable.HashSet

class SimplifierTest extends FunSuite {

  private val source = FunEqSource("test")

  private val zero = FunEqIntLeaf(0)
  private val one = FunEqIntLeaf(1)
  private val two = FunEqIntLeaf(2)
  private val three = FunEqIntLeaf(3)
  private val four = FunEqIntLeaf(4)
  private val five = FunEqIntLeaf(5)

  test("BigSimplifier - constants addition 1") {
    val x = FunEqVarLeaf("x")
    val eq1 = FunEqEquation(source, x, FunEqNode("+", two, two), isEquality = true)

    val simplified = new BigSimplifier().process(HashSet(eq1))

    val correct = FunEqEquation(source, x, four, isEquality = true)

    assert(simplified.size === 1)
    assert(simplified.head == correct)
  }

  test("BigSimplifier - constants addition 2") {

    val x = FunEqVarLeaf("x")
    val y = FunEqVarLeaf("y")
    val eq1 = FunEqEquation(
      source,
      y,
      FunEqNode("+", two, FunEqNode("+", x, three)),
      isEquality = true
    )

    val simplified = new BigSimplifier().process(HashSet(eq1))

    val correct = FunEqEquation(
      source,
      y,
      FunEqNode("+", x, five),
      isEquality = true
    )

    assert(simplified.size === 1)
    assert(simplified.head == correct)
  }

  // f(2) * f(2) = f(2 + 2) + (2 * 2) -> f(2) * f(2) = f(4) + 4
  test("BigSimplifier - constants addition 3") {

    val eq1 = FunEqEquation(
      source,
      FunEqNode("*", FunEqFunc("f", two), FunEqFunc("f", two)),
      FunEqNode("+",
        FunEqFunc("f", FunEqNode("+", two, two)),
        FunEqNode("*", two, two)
      ),
      isEquality = true
    )

    val simplified = new BigSimplifier().simplify(eq1)

    val correct = FunEqEquation(
      source,
      FunEqNode("*", FunEqFunc("f", two), FunEqFunc("f", two)),
      FunEqNode("+",
        FunEqFunc("f", four),
        four
      ),
      isEquality = true
    )

    assert(simplified == correct)
  }

  test("BigSimplifier - constants addition 4") {

    val eq1 = FunEqEquation(
      source,
      FunEqFunc("f", FunEqIntLeaf(-2)),
      FunEqIntLeaf(-1),
      isEquality = true
    )

    val simplified = new BigSimplifier().simplify(eq1)

    assert(simplified == eq1)
  }

  test("Simplifier.process") {

    val eq1 = FunEqEquation(
      source,
      FunEqNode("+", five, FunEqFunc("f", FunEqVarLeaf("x"))),
      FunEqNode("+", four, FunEqFunc("f", FunEqVarLeaf("y"))),
      isEquality = true
    )

    val simplified = new BigSimplifier().simplify(eq1)

    val correct = FunEqEquation(
      source,
      FunEqNode("+", one, FunEqFunc("f", FunEqVarLeaf("x"))),
      FunEqFunc("f", FunEqVarLeaf("y")),
      isEquality = true
    )

    assert(simplified == correct)
  }
}
