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
import org.junit.Test
import scalax.file.Path

abstract class FsSeekableTests extends AbstractSeekableTests[Path] with Fixture {
  def open(data : String, closeAction:CloseAction[Path]) : Seekable = {
    val path = fixture.path
    path write data
    path
  }
  // can't do the close action for paths so... 
  override def correctly_closes_resources = ()
  
}
