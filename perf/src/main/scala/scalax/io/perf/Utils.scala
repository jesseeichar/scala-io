package scalax.io.perf
import scalax.io.Line.Terminators.NewLine
import util.Random._
object Utils {
  def generateTestData(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val lineStrings = 1 to lines map { _ =>
      nextString(size).replaceAll("\n", " ")
    }
    lineStrings mkString term
  }
}