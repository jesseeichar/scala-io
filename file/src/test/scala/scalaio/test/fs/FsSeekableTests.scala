/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
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
import java.io.IOException

abstract class FsSeekableTests extends AbstractSeekableTests[Path] with Fixture with SeekableTestUtils[Path] {
  def forceErrorOnAccess() = {
    fixture.root.***.filter(_.isFile).foreach{
      path =>
        path .access = ""
    }
    fixture.root.access = ""
  }
  def openResource(openFunction: () => Unit, closeAction: CloseAction[Path]): Seekable = {
    fixture.root.*("*").foreach(_.deleteRecursively(true,true))
    val path = fixture.path
    path.createFile(true)
    path
  }

  override def scalaIoException_On_Write_Error_by_default{
    intercept[IOException] {
      errorOnWriteOut.write("hi")
    }
  }
  override def scalaIoException_On_Read_Error_by_default{
    intercept[IOException] {
      errorOnWriteOut.write("hi")
    }
  }
  def canAddCloseAction = false
  override def canExecuteOpenFunction = false
  // can't do the close action for paths so...
  override def correctly_closes_resources = ()
  override def input_closed_after_each_action = ()
  override def scalaIoException_On_Write_Close_Error_by_default = ()
  override def scalaIoException_On_Close_Error_by_default = ()

}
