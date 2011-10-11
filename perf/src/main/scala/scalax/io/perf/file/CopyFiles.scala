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
import scalax.test.sugar._
import sperformance.Keys
import scalax.file.Path
import scalaio.test.fs.defaultfs.DefaultFixture

class TraverseDirTreePerformanceTest extends PerformanceDSLTest with DefaultFixture{
  val NumFiles = 30
  val root = Path.roots.head
  before() // setup fixture
  val treeRoot = fixture.tree(NumFiles,5)._1

  performance of "Path" in {
    having attribute (Keys.WarmupRuns -> 1) in {
      measure method "***" in {
        withSize from (NumFiles) upTo NumFiles withSetup { i =>
          ()
        } run { max =>
          (treeRoot.***).foreach(d => ())
        }
      }
    }
  }
  
  override def tearDown = {
    fixture.after()
  }

}

object CopyFilesPerformanceTest {
  def main(args: Array[String]) {
    Main.runTests(() => new TraverseDirTreePerformanceTest)
  }
}
