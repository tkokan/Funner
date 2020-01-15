import java.io.File
import funTests.{FunTest, FunTestParser}
import org.scalatest.FunSuite
import processor.BigProcessor
import scala.collection.immutable.HashSet


class JsonTests extends FunSuite {

  private val testsFolder = "jsonTests"
  private val path = getClass.getResource(testsFolder).getPath
  private val folder = new File(path)

  private def getTestsFromFile(filename: String): List[FunTest] = {
    val source = scala.io.Source.fromFile(filename)
    val json = try source.mkString finally source.close()

    FunTestParser.parseTests(json)
  }

  private def runTestsFromFile(filename: String): Unit =
    getTestsFromFile(filename).zipWithIndex.foreach(t => runTest(t._1, t._2 + 1, filename))

  private def runTest(t: FunTest, index: Int, filename: String): Unit = {
    test(t.name) {

      val assumptions = t.sources.filter(!_.isEquality)
      val equations = HashSet() ++ t.sources.filter(_.isEquality)

      val simplified = new BigProcessor(assumptions).process(equations)

      assert(simplified.contains(t.target))
    }
  }

  folder.listFiles
    .toList
    .foreach(file => runTestsFromFile(file.getPath))
}
