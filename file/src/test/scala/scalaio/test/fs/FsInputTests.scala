/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.fs

import scalax.io._
import scalaio.test._
import org.junit.Test
import java.io.IOException
import scalaio.test.AbstractInputTests._

abstract class FsInputTests extends AbstractInputTests with Fixture {
    protected def input(t:Type, closeFunction: () => Unit) = t match {
        case t @ TextNewLine => fixture.text(t.sep)
        case t @ TextPair => fixture.text(t.sep)
        case t @ TextCarriageReturn => fixture.text(t.sep)
        case TextCustom(sep) => fixture.text(sep)
        case TextCustomData(sep,data) =>
          val p = fixture.path
          p.outputStream().writer(Codec.UTF8).write(data)
          p
        case Image => fixture.image
       case ErrorOnRead => fixture.errorOnAccessResource
       case ErrorOnClose => fixture.errorOnCloseResource
    }
    
  @Test
  override def scalaIoException_On_Read_Error_by_default {
    intercept[IOException] {
      input(ErrorOnRead).bytes.head
    }
  }
  
  // can't do the close action for paths so...
  override def input_closed_after_each_action {}
}
