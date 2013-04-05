/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs.defaultfs

import scalax.file.FileSystem
import org.junit.Test
import org.junit.Ignore
import scala.util.control.Breaks

class RandomPrefixTest {
  @Test
  def `windows paths cannot end in .` {
    0 to 500 foreach {i =>
      val prefix = FileSystem.default.randomPrefix
      assert(prefix(prefix.size - 1) != '.', "'"+prefix+"' should not end in a .")
    }
  }
}
class FsMatchingTest extends scalaio.test.fs.FsMatchingTests with DefaultFixture
class FsDirectoryStreamTest extends scalaio.test.fs.FsPathSetTests with DefaultFixture {
    @Test //@Ignore
    def stackOverflowBug : Unit = {
      val testTime = 30000
      println("start overflow bug test")
      FileSystem.default.roots.headOption foreach {path =>
          val start = System.currentTimeMillis()
          var dots:Seq[Int] = 1 to 100
          Breaks.breakable {
            (path.***).foreach { p =>
              val elapsed = (System.currentTimeMillis() - start)
              if (dots.head == Math.round(elapsed / 1000)) {
                if (dots.head % 5 == 0) {
                  print(" "+(Math.round(testTime - elapsed)/1000)+"s ")
                } else {
            	  print('.')
                }
            	dots = dots.tail
              }
              
              if (elapsed > testTime) Breaks.break;
            }
          }
      }
      println("\ndone overflow bug test and it passed... yay")
      // no error is a pass
  }

}
class FsFileOpsTest extends scalaio.test.fs.FsFileOpsTests with DefaultFixture {
  @Test
  def placeholder = ()
}
class FileSystemTest extends scalaio.test.fs.FsFileSystemTests with DefaultFixture
class SeekableTest extends scalaio.test.fs.FsSeekableTests with DefaultFixture
class BasicPathTest extends scalaio.test.fs.FsBasicPathTests with DefaultFixture
class AccessSetTest extends scalaio.test.fs.FsAccessSetTests with DefaultFixture
class PathObjectTest extends scalaio.test.fs.FsPathObjectTests with DefaultFixture
class PathSetTest extends scalaio.test.fs.FsPathSetTests with DefaultFixture
