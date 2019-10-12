//package processor.simplifier
//
//import general.{FunEqEquation, FunEqFunc, FunEqIntLeaf}
//import scala.annotation.tailrec
//import scala.collection.immutable.HashSet
//
//object AllSimpleSimplifier extends SimpleSimplifier {
//
//  @tailrec
//  override def simplify(equation: FunEqEquation) : Option[FunEqEquation] = {
//    val processed = composed(Some(equation))
//
//    processed match {
//      case Some(x) if x != equation => simplify(x)
//      case _ => processed
//    }
//  }
//
//  private val simplifiers: List[Simplifier] = List()
//
//  @tailrec
//  // Returns (functionValues, equations)
//  def process(functionValues: HashSet[FunEqEquation], equations: HashSet[FunEqEquation])
//  : (HashSet[FunEqEquation], HashSet[FunEqEquation]) = {
//
//    val (newFunctionValues, newEquations) = equations.partition(isFunctionValue)
//    val combinedFunctionValues = newFunctionValues ++ functionValues
//    val processed = newEquations.flatMap(simplify)
//
//    if (processed == equations && combinedFunctionValues == functionValues)
//      (functionValues, equations)
//    else
//      process(combinedFunctionValues, processed)
//  }
//
//  // This assumes normal form
//  private def isFunctionValue(equation: FunEqEquation): Boolean = equation match {
//    case FunEqEquation(_, FunEqFunc(_, FunEqIntLeaf(_)), FunEqIntLeaf(_)) => true
//    case _ => false
//  }
//
//  private lazy val composed: Option[FunEqEquation] => Option[FunEqEquation] =
//    simplifiers
//      .map(x => (eq: Option[FunEqEquation]) => eq.flatMap(x.simplify))
//      .foldLeft(identity[Option[FunEqEquation]] _)(_ compose _)
//}
