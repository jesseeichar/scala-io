/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.defaultfs

import util.Random

import scalax.io._
import scalax.io.resource._

import org.junit.rules.TemporaryFolder
import java.io.InputStream
import scalaio.test._

class DefaultFileSystemFixture(val folder : TemporaryFolder, rnd : Random = new Random())
  extends FileSystemFixture(FileSystem.default, rnd) {
    folder.create()

    override val root = Path(folder.getRoot)
    override def after = folder.delete()
}
