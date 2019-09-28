package parser

import scala.util.parsing.combinator._

class FunEqParser extends JavaTokenParsers with RegexParsers {

  def eq: Parser[FunEqTreeEquation] = expr~"="~expr ^^
    { case left~"="~right => FunEqTreeEquation(left, right) }

  def expr: Parser[FunEqTreeExpr] = (
    term~("+"|"-")~term ^^ { case left~op~right => FunEqTreeNode(op, left, right) }
      | term
    )

  def term: Parser[FunEqTreeExpr] = (
    factor~"*"~factor ^^ { case left ~ "*" ~ right => FunEqTreeNode("*", left, right) }
      | factor ~ "*" ~ "(" ~ expr ~ ")" ^^ { case left ~ "*" ~ "(" ~ right ~ ")" => FunEqTreeNode("*", left, right) }
      | "(" ~ expr ~ ")" ~ "*" ~  factor ^^ { case "(" ~ left ~ ")" ~ "*" ~ right => FunEqTreeNode("*", left, right) }
      | factor
    )

  def factor: Parser[FunEqTreeExpr] = variable | functionCall

  def variable: Parser[FunEqTreeExpr] = """[xyzw]""".r ^^
    { v => FunEqTreeVarLeaf(v) }

  def functionCall: Parser[FunEqTreeExpr] = "f" ~ "(" ~ expr ~ ")" ^^
    { case "f" ~ "(" ~ arg ~ ")" => FunEqTreeFunc("f", arg) }

}