package parser

import scala.util.parsing.combinator._
import java.util.UUID.randomUUID

class FunEqParser extends JavaTokenParsers with RegexParsers {

  def eq: Parser[FunEqEquation] = expr~"="~expr ^^
    { case left~"="~right => FunEqEquation(randomUUID.toString, List(), left, right) }

  def expr: Parser[FunEqExpr] = (
    term~("+"|"-")~term ^^ { case left~op~right => FunEqNode(op, left, right) }
      | term
    )

  def term: Parser[FunEqExpr] = (
    factor~"*"~factor ^^ { case left ~ "*" ~ right => FunEqNode("*", left, right) }
      | factor
    )

  def factor: Parser[FunEqExpr] = variable | const | functionCall | "(" ~> expr <~ ")"

  def variable: Parser[FunEqExpr] = """[xyzw]""".r ^^ { v => FunEqVarLeaf(v) }

  def const: Parser[FunEqExpr] = wholeNumber ^^ { v => FunEqVarLeaf(v) }

  def functionCall: Parser[FunEqExpr] = "f" ~ "(" ~ expr ~ ")" ^^
    { case "f" ~ "(" ~ arg ~ ")" => FunEqFunc("f", arg) }

  def parseEquation(input: String): ParseResult[FunEqEquation] = parseAll(eq, input)

}