package scalax.io.perf
package reader

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

object SmallMediumSetsFromFileReaderCharsTest extends AbstractReaderCharsTest {

  val MaxSize = 15000
  val Inc = 5000
  val From = 5000
  val WarmUpRuns = 100

  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val data = generateTestData(size, lines, term)
    val file = File.createTempFile(getClass().getSimpleName(), "txt")
    FileUtils.writeStringToFile(file, data, "UTF-8")
    () => new InputStreamReader(new FileInputStream(file),"UTF-8")
  }

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}