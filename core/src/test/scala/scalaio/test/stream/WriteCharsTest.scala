/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.stream

import scalaio.test._
import scalax.io._
import org.junit.Test
import org.junit.Assert._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PipedInputStream, PipedOutputStream}
import java.io.OutputStreamWriter
import scalax.io.managed.WriterResource

class WriteCharsTest extends AbstractWriteCharsTests {
  def open(ca:CloseAction[java.io.Writer] = CloseAction.Noop) = {

    val out = new ByteArrayOutputStream()
    def in = new ByteArrayInputStream(out.toByteArray)

    val inResource = Resource.fromInputStream(in).reader(Codec.UTF8)
    val outResource = Resource.fromWriter(new OutputStreamWriter(out)).updateContext(_.copy(closeAction=ca))

    (inResource, outResource)
  }
}
