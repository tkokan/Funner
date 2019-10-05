package processor.simplifier

import processor.{HighLevelProcessor, Processor}

object Simplifier extends HighLevelProcessor {

  override protected val processors: List[Processor] = List(
    AdditionSimplifier,
    ConstantsSimplifier,
    SumsSimplifier,
    VariablesNormaliser,
    MultiplicationSimplifier
  )
}
