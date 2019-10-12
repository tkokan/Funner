package processor

import general.FunEqEquation
import processor.combinator.Combinator
import processor.remover.Remover
import processor.simplifier.BigSimplifier

// ToDo: Should be renamed to BigProcessor
class MainProcessor(assumptions: List[FunEqEquation]) extends HighLevelProcessor {
  override protected val processors: List[Processor] = List(
    Combinator,
    new BigSimplifier(assumptions),
    Remover
  )
}
