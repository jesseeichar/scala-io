/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs.ram


class FsMatchingTest extends scalaio.test.fs.FsMatchingTests with ForwardSlashRamFixture
class FsDirectoryStreamTest extends scalaio.test.fs.FsPathSetTests with ForwardSlashRamFixture
class FsInputTest extends scalaio.test.fs.FsInputTests with ForwardSlashRamFixture
class FsOutputTest extends scalaio.test.fs.FsOutputTests with ForwardSlashRamFixture 
class FsFileOpsTest extends scalaio.test.fs.FsFileOpsTests with ForwardSlashRamFixture
class FileSystemTest extends scalaio.test.fs.FsFileSystemTests with ForwardSlashRamFixture
class SeekableTest extends scalaio.test.fs.FsSeekableTests with ForwardSlashRamFixture
class BasicPathTest extends scalaio.test.fs.FsBasicPathTests with ForwardSlashRamFixture
class AccessSetTest extends scalaio.test.fs.FsAccessSetTests with ForwardSlashRamFixture
class PathObjectTest extends scalaio.test.fs.FsPathObjectTests with ForwardSlashRamFixture
class PathFinderTest extends scalaio.test.fs.FsPathFinderTests with ForwardSlashRamFixture

