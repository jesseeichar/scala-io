/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import scalax.io.resource._
import Path.AccessModes._
import scalax.test.sugar._

import org.junit.Assert._
import org.junit.{
  Test, Before, After, Rule, Ignore
}
import org.junit.rules.TemporaryFolder
import util.Random

import java.io.IOException

class InputStreamResourceTest extends AssertionSugar with IOSugar{
    implicit val codec = Codec.UTF8

    val source = "sample"
    def resource = Resource.fromInputStream(source.inputStream)

    @Test
    def should_read_bytes = {
        val byteArray = (resource.bytesAsInts map {_.toByte}).toArray
        assertEquals(source, new String(byteArray))
    }

    @Test
    def size_should_return_None = assertEquals(None, resource.size)

    @Test
    def should_convert_to_chars = {
        val chars = resource.reader.chars
        assertEquals(source, chars.mkString)
    }

}