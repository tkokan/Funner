package parser

object ParseExpr extends FunEqParser {
  def main(args: Array[String]) {
    def input = args(0)
    def parsed = parseAll(eq, input)

    println("input: " + input)
    println(parsed)
  }
}