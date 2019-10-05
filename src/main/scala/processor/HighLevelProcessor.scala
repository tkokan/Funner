package processor

import general.FunEqEquation
import scala.annotation.tailrec
import scala.collection.immutable.HashSet

abstract class HighLevelProcessor extends Processor {

  protected val processors: List[Processor]

  @tailrec
  final override def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] = {
    val processed = composed(equations)

    if (processed == equations)
      processed
    else
      process(processed)
  }

  // This needs to be lazy so that processors are populated before this gets executed.
  private lazy val composed = processors
    .map(x => (y: HashSet[FunEqEquation]) => x.process(y))
    .foldLeft((x: HashSet[FunEqEquation]) => x)(_ compose _)
}
