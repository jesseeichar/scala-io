package scalax.io

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
import scalax.test.sugar._

object LargeSetsFromFileInputTest extends AbstractInputTest {

  val MaxSize = 1000000 
  val Inc = 1
  val From = 1
  val WarmUpRuns = 1

  // create Test File once for all tests
  val file = LargeResource.largeResource(KEY.TEXT)
  
  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val actualFile = if (term == NewLine.sep) {
      file
    } else {
      LargeResource.largeResource("LargeSetsFromFileInputTest-" + term) {
        output =>
          val lineStrings = 1 to lines map { _ =>
            nextString(size).replaceAll("\n", " ")
          }
          val data = lineStrings mkString term
          val file = File.createTempFile(getClass().getSimpleName(), "txt")
          FileUtils.writeStringToFile(file, data, "UTF-8")
      }
    }
    () => new FileInputStream(actualFile)
  }

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}