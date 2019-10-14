package processor

import general.FunEqEquation

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

abstract class HighLevelProcessor extends Processor {

  private val maxEquations = 100

  protected val processors: List[Processor]

  @tailrec
  final override def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] = {

    println(s"$this :: ${equations.size}")

    if (equations.exists(_.isImpossible)) equations
    else {
      val processed = composed(equations)

      if (processed == equations) processed
      else if (processed.size > maxEquations) {
        println(s"Giving up - ${processed.size} equations is too many.")
        processed
      } else process(processed)
    }
  }

  // This needs to be lazy so that processors are populated before this gets executed.
  private lazy val composed = processors
    .reverse
    .map(x => (y: HashSet[FunEqEquation]) => x.process(y))
    .reduceLeft(_ compose _)
}
