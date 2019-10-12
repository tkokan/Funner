import general.{FunEqEquation, FunEqFunc, FunEqIntLeaf, FunEqNode, FunEqSource}
import org.scalatest.FunSuite
import processor.simplifier.BigSimplifier

import scala.collection.immutable.HashSet

class CancelMultiplicationSimplifierTest extends FunSuite {

  private val source = FunEqSource("test")
  private val zero = FunEqIntLeaf(0)

  test("BigSimplifier - constants addition 1") {
    val fOne = FunEqFunc("f", FunEqIntLeaf(1))
    val fMinusOne = FunEqFunc("f", FunEqIntLeaf(-1))

    val eq1 = FunEqEquation(source,
      FunEqNode("*", fMinusOne, fOne),
      zero,
      isEquality = true
    )

    val assumption = FunEqEquation(source, fMinusOne, FunEqIntLeaf(0), isEquality = false)

    val simplified = new BigSimplifier(List(assumption)).simplify(eq1)

    val correct = FunEqEquation(source, fOne, zero, isEquality = true)

    assert(simplified == correct)
  }

  test("Cancel multiplication - nothing to cancel") {
    val fTwo = FunEqFunc("f", FunEqIntLeaf(2))
    val fMinusOne = FunEqFunc("f", FunEqIntLeaf(-1))

    val eq1 = FunEqEquation(source,
      FunEqNode("*", fTwo, fMinusOne),
      FunEqIntLeaf(-2),
      isEquality = true
    )

    val assumption = FunEqEquation(source, fMinusOne, FunEqIntLeaf(0), isEquality = false)

    val simplified = new BigSimplifier(List(assumption)).simplify(eq1)

    assert(simplified == eq1)
  }
}