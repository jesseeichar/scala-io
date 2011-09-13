package scalax.io.perf
package reader

import scalax.io.perf.AbstractReadableByteChannelInputTest
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.File
import scalax.io.perf.AbstractWritableByteChannelOutputTest
import org.apache.commons.io.FileUtils
import scalax.io.Line.Terminators.NewLine
import Utils._
import java.nio.channels.Channels
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStreamReader
import java.io.OutputStreamWriter

trait MemoryBase{
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val data = generateTestData(size, lines, term)
    () => new InputStreamReader(new ByteArrayInputStream(data.getBytes), "UTF-8")
  }
  def newOut = {
    () => new OutputStreamWriter(new ByteArrayOutputStream())
  }


}