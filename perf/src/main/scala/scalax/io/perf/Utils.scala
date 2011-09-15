package scalax.io.perf
import scalax.io.Line.Terminators.NewLine
import util.Random._
object Utils {
  def generateTestData(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val data = new StringBuilder()
    1 to lines foreach { _ =>
      1 to size foreach { _ => data.append(util.Random.alphanumeric.head) }
    }
    data.toString
  }
}