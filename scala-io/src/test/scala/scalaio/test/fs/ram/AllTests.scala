/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs.ram


class FsMatchingTest extends scalaio.test.fs.FsMatchingTests with RamFixture
class FsDirectoryStreamTest extends scalaio.test.fs.FsDirectoryStreamTests with RamFixture 
class FsInputTest extends scalaio.test.fs.FsInputTests with RamFixture
class FsOutputTest extends scalaio.test.fs.FsOutputTests with RamFixture 
class FsFileOpsTest extends scalaio.test.fs.FsFileOpsTests with RamFixture
class FileSystemTest extends scalaio.test.fs.FsFileSystemTests with RamFixture
class SeekableTest extends scalaio.test.fs.FsSeekableTests with RamFixture
class BasicPathTest extends scalaio.test.fs.FsBasicPathTests with RamFixture
class AccessSetTest extends scalaio.test.fs.FsAccessSetTests with RamFixture
