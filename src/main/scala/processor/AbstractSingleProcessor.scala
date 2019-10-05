//package processor
//
//import general.FunEqEquation
//
//import scala.collection.immutable.HashSet
//
//abstract class AbstractSingleProcessor extends Processor {
//
//  override def process(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] = {
//    val simplified = equations.map(process)
//
//    simplified.filter(x => !x.isTrivial)
//  }
//
//  @tailrec
//  private def simplify(equation: FunEqEquation): FunEqEquation = {
//    val simplified = composedSimplifier(equation)
//
//    if (simplified == equation)
//      simplified
//    else
//      simplify(simplified)
//  }
//
//  def simplify(equations: HashSet[FunEqEquation]): HashSet[FunEqEquation] =
//
//
//}