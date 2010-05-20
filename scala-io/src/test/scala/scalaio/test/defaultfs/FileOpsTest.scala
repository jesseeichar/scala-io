/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.defaultfs

import scalax.io._
import scalaio.test.AbstractFileOpsTests
import org.junit.Assert._
import org.junit.{
  Test, Ignore
}

import java.io.IOException

class FileOpsTest extends AbstractFileOpsTests with DefaultFixture {

  def ops(implicit data : Array[Byte]) = {
    val path = fixture.path
    path.createFile()
    val ops = path.ops
    ops write data
    ops
  }

}