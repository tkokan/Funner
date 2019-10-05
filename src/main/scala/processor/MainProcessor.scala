package processor

import processor.combinator.Combinator
import processor.simplifier.Simplifier

object MainProcessor extends HighLevelProcessor {
  override protected val processors: List[Processor] = List(Simplifier, Combinator)
}
