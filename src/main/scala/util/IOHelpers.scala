package util

import scala.io.Source._

object IOHelpers {

  // In
  def getResourceLines(resourceFile: String): List[String] = {
    val fileStream = getClass.getResourceAsStream(resourceFile)
    fromInputStream(fileStream).getLines.toList
  }

}
