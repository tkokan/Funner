package processor.combinator

import processor.{HighLevelProcessor, Processor}

object Combinator extends HighLevelProcessor {

  override protected val processors: List[Processor] = List(
    ConstantsSubstitutor,
    EquationsSubsitutor,
    VariablesShifter)
}
