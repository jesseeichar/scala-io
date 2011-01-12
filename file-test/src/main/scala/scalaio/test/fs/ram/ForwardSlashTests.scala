/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs.ram


class ForwardSlashFsMatchingTest extends scalaio.test.fs.FsMatchingTests with ForwardSlashRamFixture
class ForwardSlashFsDirectoryStreamTest extends scalaio.test.fs.FsPathSetTests with ForwardSlashRamFixture
class ForwardSlashFsInputTest extends scalaio.test.fs.FsInputTests with ForwardSlashRamFixture
class ForwardSlashFsOutputTest extends scalaio.test.fs.FsOutputTests with ForwardSlashRamFixture
class ForwardSlashFsFileOpsTest extends scalaio.test.fs.FsFileOpsTests with ForwardSlashRamFixture
class ForwardSlashFileSystemTest extends scalaio.test.fs.FsFileSystemTests with ForwardSlashRamFixture
class ForwardSlashSeekableTest extends scalaio.test.fs.FsSeekableTests with ForwardSlashRamFixture
class ForwardSlashBasicPathTest extends scalaio.test.fs.FsBasicPathTests with ForwardSlashRamFixture
class ForwardSlashAccessSetTest extends scalaio.test.fs.FsAccessSetTests with ForwardSlashRamFixture
class ForwardSlashPathObjectTest extends scalaio.test.fs.FsPathObjectTests with ForwardSlashRamFixture
class ForwardSlashPathFinderTest extends scalaio.test.fs.FsPathFinderTests with ForwardSlashRamFixture

