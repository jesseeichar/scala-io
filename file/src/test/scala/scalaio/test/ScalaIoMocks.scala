/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io.FileSystem
import scalax.io.ramfs.RamFileSystem
import scalax.test.sugar.MockitoSugarSupport.mock

object ScalaIoMocks {
  def fileSystemMock:FileSystem = new RamFileSystem
}
