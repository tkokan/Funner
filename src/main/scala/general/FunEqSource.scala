package general

case class FunEqSource(description: String, parents: List[FunEqEquation] = Nil) {

  override def toString: String = description
}
