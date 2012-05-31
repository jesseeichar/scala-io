package scalax.io.perf
package channel

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

import org.apache.commons.io.FileUtils
import _root_.scalax.io.Line.Terminators.NewLine
import Utils._

trait FileBase {
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    val data = generateTestData(size, lines, term)
    FileUtils.writeStringToFile(file, data, "UTF-8")
    () => new FileInputStream(file).getChannel()
  }
  def newOut = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    () => new FileOutputStream(file).getChannel()
  }


}
