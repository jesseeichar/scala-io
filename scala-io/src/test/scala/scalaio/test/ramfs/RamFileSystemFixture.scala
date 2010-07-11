/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.ramfs

import util.Random

import scalax.io._
import scalax.io.resource._
import scalax.io.ramfs._

import org.junit.rules.TemporaryFolder
import java.io.InputStream
import scalaio.test._

class RamFileSystemFixture(rnd : Random = new Random())
      extends FileSystemFixture(new RamFileSystem(), rnd) {
  override val root = fs.roots.head
}