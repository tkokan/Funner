package funTests

import parser.FunEqParser
import spray.json._
import FunTestRawJsonProtocol._

object FunTestParser {
  val parser = new FunEqParser()

  def parseTests(json: String): List[FunTest] = {

    // abstract syntax tree
    val jsonAst = json.parseJson
    val rawTests = jsonAst.convertTo[List[FunTestRaw]]

    rawTests.map(r => FunTest(
      name = r.name,
      sources = r.sources.map(s => parser.parseEquation(s).get),
      target = parser.parseEquation(r.target).get))
  }
}
