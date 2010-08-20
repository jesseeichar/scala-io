/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs.ram


class RamFsMatchingTest extends scalaio.test.fs.FsMatchingTests with RamFixture
class RamFsDirectoryStreamTest extends scalaio.test.fs.FsDirectoryStreamTests with RamFixture 
class RamFsInputTest extends scalaio.test.fs.FsInputTests with RamFixture
class RamFsOutputTest extends scalaio.test.fs.FsOutputTests with RamFixture 
class RamFsFileOpsTest extends scalaio.test.fs.FsFileOpsTests with RamFixture
/*
class RamFileSystemTest extends scalaio.test.fs.FsFileSystemTests with RamFixture
*/
class RamSeekableTest extends scalaio.test.fs.FsSeekableTests with RamFixture
