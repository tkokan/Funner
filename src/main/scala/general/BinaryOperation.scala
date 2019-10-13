package general

object BinaryOperation extends Enumeration {
  type BinaryOperation = Value
  val + : BinaryOperation.Value = Value("+")
  val * : BinaryOperation.Value = Value("*")

  def neutralElement(operation: BinaryOperation): Int = {
    operation match {
      case BinaryOperation.+ => 0
      case BinaryOperation.* => 1
    }
  }
}

