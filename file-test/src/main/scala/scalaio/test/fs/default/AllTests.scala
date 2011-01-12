/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs.default

import scalax.file.FileSystem
import org.junit.Test

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
class FsDirectoryStreamTest extends scalaio.test.fs.FsPathSetTests with DefaultFixture
class FsInputTest extends scalaio.test.fs.FsInputTests with DefaultFixture
class FsOutputTest extends scalaio.test.fs.FsOutputTests with DefaultFixture
class FsFileOpsTest extends scalaio.test.fs.FsFileOpsTests with DefaultFixture
class FileSystemTest extends scalaio.test.fs.FsFileSystemTests with DefaultFixture
class SeekableTest extends scalaio.test.fs.FsSeekableTests with DefaultFixture
class BasicPathTest extends scalaio.test.fs.FsBasicPathTests with DefaultFixture
class AccessSetTest extends scalaio.test.fs.FsAccessSetTests with DefaultFixture
class PathObjectTest extends scalaio.test.fs.FsPathObjectTests with DefaultFixture
class PathFinderTest extends scalaio.test.fs.FsPathFinderTests with DefaultFixture

