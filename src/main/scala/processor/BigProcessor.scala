package processor

import general.FunEqEquation
import processor.combinator.Combinator
import processor.remover.Remover
import processor.simplifier.BigSimplifier

class BigProcessor(assumptions: List[FunEqEquation]) extends HighLevelProcessor {
  override protected val processors: List[Processor] = List(
    Combinator,
    new BigSimplifier(assumptions),
    Remover
  )
}
