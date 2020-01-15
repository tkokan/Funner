package funTests

import spray.json.{DefaultJsonProtocol, RootJsonFormat}


object FunTestRawJsonProtocol extends DefaultJsonProtocol {
  implicit val funTestFormat: RootJsonFormat[FunTestRaw] = jsonFormat3(FunTestRaw)
}
