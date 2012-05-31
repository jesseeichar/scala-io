package scalax.io.perf
package channel

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.channels.Channels
import _root_.scalax.io.Line.Terminators.NewLine

import Utils._

trait MemoryBase {
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val data = generateTestData(size, lines, term)
    () => Channels.newChannel(new ByteArrayInputStream(data.getBytes))
  }
  def newOut = {
    () => Channels.newChannel(new ByteArrayOutputStream())
  }


}
