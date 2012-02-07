/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.stream

import scalaio.test._
import java.io.{
ByteArrayInputStream, ByteArrayOutputStream
}
import org.junit.Test
import org.junit.Assert._
import scalax.io._
import java.sql.Date
import scalax.io.managed.OutputStreamResource

class OutputTest extends AbstractOutputTests[ByteArrayInputStream,ByteArrayOutputStream] {
  def open(closeAction:CloseAction[ByteArrayOutputStream] = CloseAction.Noop) = {
    val cache = new Array[Byte](1000)
    val out = new ByteArrayOutputStream()
    def in = new ByteArrayInputStream(out.toByteArray)

    val inResource = Resource.fromInputStream(in)
    val outResource = Resource.fromOutputStream(out).addCloseAction(closeAction)

    (inResource, outResource)
  }


}
