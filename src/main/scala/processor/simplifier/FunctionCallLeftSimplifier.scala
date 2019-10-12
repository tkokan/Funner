//package processor.simplifier
//
//import general.{FunEqEquation, FunEqExpression, FunEqFunc, FunEqIntLeaf, FunEqNode, FunEqSource}
//import processor.IndependentProcessor
//
//import scala.collection.immutable.HashSet
//
//object FunctionCallLeftSimplifier extends IndependentProcessor {
//
//  // f(x) + a = b => f(x) = b - a
//  override def process(equation: FunEqEquation): HashSet[FunEqEquation] = {
////    println(s"\t\t::: $equation")
//
//    val solution = equation match {
//      case FunEqEquation(source, FunEqNode("+", a, FunEqFunc(name, argument)), b)
//        if noFunctionCalls(a) && noFunctionCalls(b)
//      => createSet(source, name, argument, a, b)
//
//      case FunEqEquation(source, FunEqNode("+", FunEqFunc(name, argument), a), b)
//        if noFunctionCalls(a) && noFunctionCalls(b)
//      => createSet(source, name, argument, a, b)
//
//      case FunEqEquation(source, b, FunEqNode("+", a, FunEqFunc(name, argument)))
//        if noFunctionCalls(a) && noFunctionCalls(b)
//      => createSet(source, name, argument, a, b)
//
//      case FunEqEquation(source, b, FunEqNode("+", FunEqFunc(name, argument), a))
//        if noFunctionCalls(a) && noFunctionCalls(b)
//      => createSet(source, name, argument, a, b)
//
//      case FunEqEquation(source, b, FunEqFunc(name, argument))
//        if noFunctionCalls(b)
//      => HashSet(FunEqEquation(source, FunEqFunc(name, argument), b))
//
//      case _ => HashSet(equation)
//    }
//
////    println(s"\t\t -> $solution")
//
//  solution
//
//
//  }
//
//  private def createSet(source: FunEqSource, name: String, argument: FunEqExpression, a: FunEqExpression, b: FunEqExpression): HashSet[FunEqEquation] =
//    HashSet(FunEqEquation(source, FunEqFunc(name, argument), FunEqNode("+", b, FunEqNode("*", FunEqIntLeaf(-1), a))))
//
//  private def noFunctionCalls(expression: FunEqExpression): Boolean = expression match {
//    case FunEqFunc(_, _) => false
//    case FunEqNode(_, a, b) => noFunctionCalls(a) && noFunctionCalls(b)
//    case _ => true
//  }
//}
