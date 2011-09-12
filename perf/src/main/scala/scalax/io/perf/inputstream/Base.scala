package scalax.io.perf
package inputstream

import scalax.io.perf.AbstractReadableByteChannelInputTest
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.File
import scalax.io.perf.AbstractWritableByteChannelOutputTest
import org.apache.commons.io.FileUtils
import scalax.io.Line.Terminators.NewLine
import Utils._

abstract class Base extends AbstractInputTest with AbstractOutputTest {

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