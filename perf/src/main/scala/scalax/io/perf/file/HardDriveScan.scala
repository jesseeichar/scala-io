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

object HardDriveScan extends PerformanceDSLTest {
  val root = Path.roots.head

  performance of "Path" in {
    having attribute (Keys.WarmupRuns -> 1) in {
      measure method "**" in {
        withSize from (10) upTo 10000 by 200 withSetup { i =>
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
      measure method "**" in {
        having attribute ("version", "java.io while loop") in {
          withSize from (10) upTo 10000 by 200 withSetup { i =>
            var jroot = File.listRoots().find(_.getName == root.name).get
            (i, jroot)
          } run {
            case (max, jroot) =>
              var iter: Iterator[File] = Iterator.empty
              var waiting = Seq(jroot).iterator
              var i = 0
              while (waiting.hasNext && i < max) {
                iter = waiting
                waiting = Iterator.empty
                while (iter.hasNext || i < max) {
                  val current = iter.next
                  val children = current.listFiles
                  if (children != null) waiting ++= children.iterator

                  if (current.getName contains '.') {
                    i += 1
                  }
                }
              }
          }
        }
      }
    }
  }
 def main(args: Array[String]) {
    Main.runTests(this)
  }

}