/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test.stream

import scalax.io.resource.Resource
import scalax.io.{
    Input,Output
}
import scalax.io.Line.Terminators._
import scalax.io.Codec
import scalaio.test._

import java.io.{
    ByteArrayInputStream, ByteArrayOutputStream
}

class OutputTest extends AbstractOutputTests {
  def open() : (Input, Output) = {
    val cache = new Array[Byte](1000)
    val out = new ByteArrayOutputStream()
    def in = new ByteArrayInputStream(out.toByteArray)

    val inResource = Resource.fromInputStream(in)
    val outResource = Resource.fromOutputStream(out)

    (inResource, outResource)
  }
}