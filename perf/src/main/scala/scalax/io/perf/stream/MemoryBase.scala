package scalax.io.perf.stream

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.File
import org.apache.commons.io.FileUtils
import scalax.io.Line.Terminators.NewLine
import scalax.io.perf.Utils._
import java.nio.channels.Channels
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

trait MemoryBase {
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val data = generateTestData(size, lines, term)
    () => new ByteArrayInputStream(data.getBytes)
  }
  def newOut = {
    () => new ByteArrayOutputStream()
  }


}