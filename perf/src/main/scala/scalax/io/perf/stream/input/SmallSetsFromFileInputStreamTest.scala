package scalax.io.perf
package stream
package input

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

class SmallSetsFromFileInputStreamTest
	extends AbstractInputTest
	with FileBase {

  val MaxSize = 50
  val Inc = 25
  val From = 1
  val WarmUpRuns = 100
  val WarmUpRunsForLines = 100

}

object SmallSetsFromFileInputStreamTest {
  def main(args: Array[String]) {
    Main.runTests(() => new SmallSetsFromFileInputStreamTest)
  }
}
