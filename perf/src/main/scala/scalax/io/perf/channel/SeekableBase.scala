package scalax.io.perf
package channel

import _root_.scalax.io.perf.AbstractReadableByteChannelInputTest
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.File
import _root_.scalax.io.perf.AbstractWritableByteChannelOutputTest
import org.apache.commons.io.FileUtils
import _root_.scalax.io.Line.Terminators.NewLine
import Utils._
import _root_.scalax.io.Input
import _root_.scalax.io.Resource
import _root_.scalax.io.nio.SeekableFileChannel

trait SeekableBase {

  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    val data = generateTestData(size, lines, term)
    FileUtils.writeStringToFile(file, data, "UTF-8")
    () => new SeekableFileChannel(new FileInputStream(file).getChannel())
  }

  def newOut = {
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    () => new SeekableFileChannel(new FileOutputStream(file).getChannel())
  }

}