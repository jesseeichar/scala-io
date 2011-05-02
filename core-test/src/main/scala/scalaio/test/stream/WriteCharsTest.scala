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

class WriteCharsTest extends AbstractWriteCharsTests {
  def open() = {

    val out = new ByteArrayOutputStream()
    def in = new ByteArrayInputStream(out.toByteArray)

    val inResource = Resource.fromInputStream(in).reader(Codec.UTF8)
    val outResource = Resource.fromOutputStream(out).writer(Codec.UTF8)

    (inResource, outResource)
  }
}
