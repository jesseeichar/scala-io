/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import scalaio.test.AbstractSeekableTests


abstract class FsSeekableTests extends AbstractSeekableTests with Fixture {
  def open(data : String) : Seekable = {
    val path = fixture.path
    path write data
    path
  }
}
