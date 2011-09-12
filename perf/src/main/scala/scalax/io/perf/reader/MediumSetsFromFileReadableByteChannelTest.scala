package scalax.io.perf
package reader

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
import java.io.File
import java.io.FileInputStream
import scalax.test.sugar.LargeResource

object MediumSetsFromFileReaderCharsTest extends AbstractReaderCharsTest {

  val MaxSize = 500000
  val Inc = 250000
  val From = 250000
  val WarmUpRuns = 1
  val WarmUpRunsForLines = 1

  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val largeFile = LargeResource.largeResource(getClass.getSimpleName + size + lines + term) { writer =>
      val data = generateTestData(size, lines, term)
      writer.write(data)
    }
    () => new InputStreamReader(new FileInputStream(largeFile), "UTF-8")
  }

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}