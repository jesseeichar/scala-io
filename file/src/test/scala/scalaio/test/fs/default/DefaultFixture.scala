/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs.default


import util.Random
import scalax.io.{
  Path, FileSystem
}
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder

import scalaio.test.fs.{
  FileSystemFixture, Fixture
}

trait DefaultFixture extends Fixture{
  val rnd : Random = new Random()

  def createFixture() = {
    val folder = new TemporaryFolder()
    new FileSystemFixture(FileSystem.default, rnd) {
      folder.create()

      override val root = Path(folder.getRoot)
      override def after = {
        folder.delete()
        Thread.sleep(500)
      }
    }
  }
}
