/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.channel

import scalaio.test._
import java.io.{
ByteArrayInputStream, ByteArrayOutputStream
}
import org.junit.Test
import org.junit.Assert._
import scalax.io._
import java.sql.Date
import java.nio.channels.Channels

class OutputTest extends AbstractOutputTests {
  def open() = {
    val cache = new Array[Byte](1000)
    val oStream = new ByteArrayOutputStream()
    val out = Channels.newChannel(oStream)
    def in = Channels.newChannel(new ByteArrayInputStream(oStream.toByteArray))

    val inResource = Resource.fromReadableByteChannel(in)
    val outResource = Resource.fromWritableByteChannel(out)

    (inResource, outResource)
  }


}
