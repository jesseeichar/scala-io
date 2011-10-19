package scalax.io.perf
package file

import scalax.io._
import sperformance.Keys.WarmupRuns
import scalax.file.PathMatcher._
import sperformance.dsl._
import util.Random._
import Resource._
import Line.Terminators._
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream }
import org.apache.commons.io.FileUtils
import org.apache.commons.io.IOUtils
import scalax.test.sugar._
import sperformance.Keys
import scalax.file.Path
import scalaio.test.fs.defaultfs.DefaultFixture
import java.io.File
import scala.annotation.tailrec

class TraverseDirTreePerformanceTest extends PerformanceDSLTest with DefaultFixture {
  val NumFiles = 30
  before() // setup fixture
  val treeRoot = fixture.tree(NumFiles, 1)._1

  performance of "Path" in {
    having attribute (Keys.WarmupRuns -> 1) in {
      measure method "***" in {
        withSize from (NumFiles) upTo NumFiles withSetup { i =>
          new FileCounter()
        } run { counter =>
          (treeRoot.***).foreach(counter)
        }
      }
      measure method "***" in {
        having attribute ("version", "java.file with loop") in {
          withSize from (NumFiles) upTo NumFiles withSetup { i =>
            ()
          } run { max =>
            var i = 0
            def visit(file: File): Unit = {
              if (file.isFile) {
                i += 1
              } else {
                i += 1
                val files = file.listFiles
                if (files != null) {
                  var j = files.size - 1
                  while (j >= 0) {
                    visit(files(j))
                    j -= 1
                  }
                }
              }
            }
            visit(new File(treeRoot.path))
          }
        }
      }
      measure method "** IsFile" in {
        withSize from (NumFiles) upTo NumFiles withSetup { i =>
          new FileCounter()
        } run { counter =>
          (treeRoot ** IsFile).foreach(counter)
        }
      }
      measure method "** IsFile" in {
        having attribute ("version", "java.file with loop") in {
          withSize from (NumFiles) upTo NumFiles withSetup { i =>
            ()
          } run { max =>
            var i = 0
            def visit(file: File): Unit = {
              if (file.isFile) {
                i += 1
              } else {
                val files = file.listFiles
                var i = files.size - 1
                while (i > 0) {
                  visit(files(i))
                  i -= 1
                }
              }
            }
            visit(new File(treeRoot.path))
          }
        }
      }
      measure method "descendants(depth=3) ** IsFile" in {
        withSize from (NumFiles) upTo NumFiles withSetup { i =>
          new FileCounter()
        } run { counter =>
          (treeRoot.descendants(depth = 3) ** IsFile).foreach(counter)
        }
      }
      measure method "descendants(depth=3) ** IsFile" in {
        having attribute ("version", "java.file with loop") in {
          withSize from (NumFiles) upTo NumFiles withSetup { i =>
            ()
          } run { max =>
            var i = 0

            def visit(file: File, depth: Int = 0): Unit = {
              if (depth > 3 && file.isFile) {
                i += 1
              } else {
                val files = file.listFiles
                if (files != null) {
                  var i = files.size - 1
                  while (i > 0) {
                    visit(files(i), depth + 1)
                    i -= 1
                  }
                }
              }
            }
            visit(new File(treeRoot.path))
          }
        }
      }
      measure method "* ⁄ * ⁄ * ⁄ ** IsFile" in {
        withSize from (NumFiles) upTo NumFiles withSetup { i =>
          new FileCounter()
        } run { counter =>
          (treeRoot * "*" * "*" * "*" ** IsFile).foreach(counter)
        }
      }
      measure method "* ⁄ * ⁄ * ⁄ ** IsFile" in {
        having attribute ("version", "java.file with loop") in {
          withSize from (NumFiles) upTo NumFiles withSetup { i =>
            ()
          } run { max =>
            var i = 0

            def visit(file: File, depth: Int = 0): Unit = {
              if (depth > 3 && file.isFile) {
                i += 1
              } else {
                val files = file.listFiles
                if (files != null) {
                  var i = files.size - 1
                  while (i > 0) {
                    visit(files(i), depth + 1)
                    i -= 1
                  }
                }
              }
            }
            visit(new File(treeRoot.path))
          }
        }
      }
    }
  }

  override def tearDown = {
    fixture.after()
  }
}

class FileCounter extends Function[Path, Unit] {
  private[this] var i = 0
  def apply(p: Path) = i += 1
  def count = i
}

object TraverseDirTreePerformanceTest {
  def main(args: Array[String]) {
    Main.runTests(() => new TraverseDirTreePerformanceTest)
  }
}
