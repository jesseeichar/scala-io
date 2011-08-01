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
import scalax.test.sugar.LargeResource

object MediumSetsFromFileInputTest extends AbstractInputTest {

  val MaxSize = 500000
  val Inc = 250000
  val From = 250000
  val WarmUpRuns = 1

  def newIn(size: Int, lines: Int = 2, term: String = NewLine.sep) = {
    val largeFile = LargeResource.largeResource(getClass.getSimpleName + size + lines + term) { writer =>
      val lineStrings = 1 to lines map { _ =>
        nextString(size).replaceAll("\n", " ")
      }
      val data = lineStrings mkString term
      writer.write( data)
    }
    () => new FileInputStream(largeFile)
  }

  def main(args: Array[String]) {
    Main.runTests(this)
  }

}