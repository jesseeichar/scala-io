/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.stream

import scalax.io.resource.Resource
import scalax.io.{
    ReadChars,WriteChars
}
import scalax.io.Line.Terminators._
import scalax.io.Codec
import scalaio.test._

import java.io.{
    PipedInputStream, PipedOutputStream
}

class WriteCharsTest extends AbstractWriteCharsTests {
    def open() : (ReadChars, WriteChars) = {

        val in = new PipedInputStream()
        val out = new PipedOutputStream(in)

        val inResource = Resource.fromInputStream(in).reader(Codec.UTF8)
        val outResource = Resource.fromOutputStream(out).writer(Codec.UTF8)

        (inResource, outResource)
    }
}
