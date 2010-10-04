/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs.ram

import util.Random

import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import scalax.io.ramfs.RamFileSystem
import scalaio.test.fs.{
  FileSystemFixture, Fixture
}

trait BasicRamFixture extends Fixture{
  val rnd = new Random()

  val sep:String
  def createFixture() = new FileSystemFixture(new RamFileSystem(separator=sep), rnd) {
    override val root = fs.roots.head
  }
}

trait ForwardSlashRamFixture extends BasicRamFixture {
  val sep = "/"
}

trait BackSlashRamFixture extends BasicRamFixture {
  val sep = "\\" 
}