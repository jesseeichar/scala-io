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
    @Test @Ignore // ignore because if it passes it does nearly a full scan of harddrive so only when the issue is known to fail do we want to test this
    def stackOverflowBug : Unit = {
      println("start overflow bug test")
      FileSystem.default.roots.headOption foreach {path =>
          val start = System.currentTimeMillis()
          val iter = (path ** "*.scala").iterator
          while(iter.hasNext && System.currentTimeMillis() - start < 30000) {
            iter.next
          }
      }
      println("done overflow bug test and it passed... yay")
      // no error is a pass
  }

}
class FsFileOpsTest extends scalaio.test.fs.FsFileOpsTests with DefaultFixture
class FileSystemTest extends scalaio.test.fs.FsFileSystemTests with DefaultFixture
class SeekableTest extends scalaio.test.fs.FsSeekableTests with DefaultFixture
class BasicPathTest extends scalaio.test.fs.FsBasicPathTests with DefaultFixture
class AccessSetTest extends scalaio.test.fs.FsAccessSetTests with DefaultFixture
class PathObjectTest extends scalaio.test.fs.FsPathObjectTests with DefaultFixture
class PathSetTest extends scalaio.test.fs.FsPathSetTests with DefaultFixture
