package scalaio.test.fs.ram

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

class BackSlashFsMatchingTest extends scalaio.test.fs.FsMatchingTests with BackSlashRamFixture
class BackSlashFsDirectoryStreamTest extends scalaio.test.fs.FsPathSetTests with BackSlashRamFixture
class BackSlashFsInputTest extends scalaio.test.fs.FsInputTests with BackSlashRamFixture
class BackSlashFsOutputTest extends scalaio.test.fs.FsOutputTests with BackSlashRamFixture
class BackSlashFsFileOpsTest extends scalaio.test.fs.FsFileOpsTests with BackSlashRamFixture
class BackSlashFileSystemTest extends scalaio.test.fs.FsFileSystemTests with BackSlashRamFixture
class BackSlashSeekableTest extends scalaio.test.fs.FsSeekableTests with BackSlashRamFixture
class BackSlashBasicPathTest extends scalaio.test.fs.FsBasicPathTests with BackSlashRamFixture
class BackSlashAccessSetTest extends scalaio.test.fs.FsAccessSetTests with BackSlashRamFixture
class BackSlashPathObjectTest extends scalaio.test.fs.FsPathObjectTests with BackSlashRamFixture
class BackSlashPathSetTest extends scalaio.test.fs.FsPathSetTests with BackSlashRamFixture

