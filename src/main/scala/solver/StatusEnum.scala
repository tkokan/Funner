package solver

object StatusEnum extends Enumeration {
  type Status = Value
  val Solved, Unsolved, Impossible = Value
}
