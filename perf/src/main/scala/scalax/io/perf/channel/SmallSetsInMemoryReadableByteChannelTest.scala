package scalax.io.perf
package channel

import Utils._
import scalax.io._
import sperformance.Keys.WarmupRuns
import sperformance.dsl._
import util.Random._
import Resource._
import Line.Terminators._
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream }
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.nio.channels.Channels

object SmallSetsInMemoryReadableByteChannelTest extends AbstractReadableByteChannelInputTest {

  val MaxSize = 50
  val Inc = 25
  val From = 1
  val WarmUpRuns = 5000
  val WarmUpRunsForLines = 1000

  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val data = generateTestData(size, lines, term)
    () => Channels.newChannel(new ByteArrayInputStream(data.getBytes))
  }

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}