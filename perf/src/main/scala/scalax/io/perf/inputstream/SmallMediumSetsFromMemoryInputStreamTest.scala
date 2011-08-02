package scalax.io.perf
package inputstream

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
import java.io.File
import java.io.FileInputStream

object SmallMediumSetsFromMemoryInputStreamTest extends AbstractInputTest {

  val MaxSize = 15000
  val Inc = 5000
  val From = 5000
  val WarmUpRuns = 10

  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val data = generateTestData(size, lines, term)
    () => new ByteArrayInputStream(data.getBytes)
  }

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}