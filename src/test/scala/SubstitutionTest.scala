import general._
import org.scalatest.FunSuite
import processor.AuxProcessor

class SubstitutionTest extends FunSuite {

  private val source = FunEqSource("test")

  test("BigSimplifier - constants addition 1") {
    val zero = FunEqIntLeaf(0)
    val fZero = FunEqFunc("f", zero)
    val one = FunEqIntLeaf(1)
    val minusOne = FunEqIntLeaf(-1)

    val eq1 = FunEqEquation(source,
      FunEqNode(
        BinaryOperation.*,
        fZero,
        FunEqNode(
          BinaryOperation.+,
          fZero,
          minusOne
        )
      ),
      zero,
      isEquality = true
    )

    val eq2 = FunEqEquation(source,
      fZero,
      one,
      isEquality = true
    )

    val inserted = AuxProcessor.substitute(eq1, fZero, one, source)

    val correct = FunEqEquation(
      source,
      zero,
      zero,
      isEquality = true
    )

    assert(inserted == correct)
  }
}
