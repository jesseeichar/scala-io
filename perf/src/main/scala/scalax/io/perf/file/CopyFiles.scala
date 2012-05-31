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

class CopyFilesPerformanceTest extends PerformanceDSLTest with DefaultFixture{
  val NumFiles = 40
  val root = Path.roots.head
  before() // setup fixture
  println("CopyFilesPerformanceTest: setting up test")
  val fromPath = fixture.tree(NumFiles,5)._1
  val text = util.Random.nextString(50)
  fromPath.***.filter (_.isFile).foreach(f => f.write(text))
  val copyPath = fixture.path(1)
  fromPath.copyTo(copyPath, replaceExisting=true)
  val toPath = fixture.path(1).createDirectory()
  println("CopyFilesPerformanceTest: done setup")

  def copyLibrary() {
    def copyFile(from: Path) {
        val toCopy = copyPath \ from.relativize(fromPath)
        val toMake = toPath \ from.relativize(fromPath)
        toMake.deleteIfExists(force = true)
        toMake.createFile()
        toCopy.copyDataTo(output = toMake)
    }

    fromPath.descendants().collect {
        case item if item.isFile => copyFile(item)
    }
}

  performance of "Path" in {
    having attribute (Keys.WarmupRuns -> 10) in {
      measure method "**" in {
        withSize from (NumFiles) upTo NumFiles withSetup { i =>
          ()
        } run { max =>
          val i = System.currentTimeMillis()
          copyLibrary()
          println(System.currentTimeMillis - i)
        }
      }
    }
  }

  override def tearDown = {
  println("CopyFilesPerformanceTest: tearingDown created files")
    fixture.after()
  }

}

object CopyFilesPerformanceTest {
  def main(args: Array[String]) {
    Main.runTests(() => new CopyFilesPerformanceTest)
  }
}
