/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.ramfs

import scalax.io._
import scalaio.test.AbstractFileOpsTests
import org.junit.Assert._
import org.junit.{
  Test, Ignore
}

import java.io.IOException

class FileOpsTest extends AbstractFileOpsTests {

  def path(implicit data : Array[Byte]) = { null // todo
  }

}