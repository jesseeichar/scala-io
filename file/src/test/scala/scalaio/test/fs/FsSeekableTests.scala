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
import scalaio.test.SeekableTestUtils

abstract class FsSeekableTests extends AbstractSeekableTests[Path] with Fixture with SeekableTestUtils[Path] {
  def deleteResource() = fixture.root.*("*").foreach(_.deleteRecursively(true,true))
  def openResource(closeAction: CloseAction[Path]): Seekable = {
    deleteResource()
    val path = fixture.path
    path
  }

  def canAddCloseAction = false
  // can't do the close action for paths so...
  override def correctly_closes_resources = ()

}
