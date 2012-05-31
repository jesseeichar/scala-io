package scalax.io.perf

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.File
import org.apache.commons.io.FileUtils
import scalax.io.Line.Terminators.NewLine
import scalax.io.perf.Utils._

trait FileBase{
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    val data = generateTestData(size, lines, term)
    FileUtils.writeStringToFile(file, data, "UTF-8")
    () => new FileInputStream(file)
  }
  def newOut = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    () => new FileOutputStream(file)
  }


}
