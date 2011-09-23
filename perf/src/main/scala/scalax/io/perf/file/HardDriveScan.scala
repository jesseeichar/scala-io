package scalax.io.perf
package file

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
import scalax.test.sugar._
import sperformance.Keys
import scalax.file.Path

class HardDriveScan extends PerformanceDSLTest {
  val root = Path.roots.head

  performance of "Path" in {
    having attribute (Keys.WarmupRuns -> 10) in {
      measure method "**" in {
        withSize from (1000) upTo 10000 by 5000 withSetup { i =>
          i
        } run { max =>
          val iter = (root ** "*.*").iterator
          var i = 0
          while (i < max && iter.hasNext) {
            iter.next
            i += 1
          }
        }
      }
    }
  }

}

object HardDriveScan {
  def main(args: Array[String]) {
    Main.runTests(() => new HardDriveScan)
  }
}
