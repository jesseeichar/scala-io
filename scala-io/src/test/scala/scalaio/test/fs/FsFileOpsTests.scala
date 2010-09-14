/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import scalaio.test.AbstractFileOpsTests
import org.junit.Assert._
import org.junit.{
  Test, Ignore
}

import java.io.IOException

abstract class FsFileOpsTests extends AbstractFileOpsTests with Fixture {

  def path(implicit data : Array[Byte]) = {
    val path = fixture.path
    path.createFile()
    val ops = path
    ops write data
    path
  }

}