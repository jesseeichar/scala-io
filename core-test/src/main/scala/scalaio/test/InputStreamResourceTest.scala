/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalaio.test

import scalax.io._
import scalax.test.sugar._

import org.junit.Assert._
import org.junit.{
Test, Before, After, Rule, Ignore
}

class InputStreamResourceTest extends AssertionSugar with IOSugar {
  implicit val codec = Codec.UTF8

  val source = "sample"

  def resource = Resource.fromInputStream(source.inputStream)

  @Test
  def should_read_bytes = {
    val byteArray = (resource.bytesAsInts map {
      _.toByte
    }).toArray
    assertEquals(source, new String(byteArray,codec.charSet))
  }

  @Test
  def size_should_return_None = assertEquals(None, resource.size)

  @Test
  def should_convert_to_chars = {
    val chars = resource.reader.chars
    assertEquals(source, chars.mkString)
  }

  @Test
  def reading_should_only_open_stream_once = {
    import Input._
    val byteArray = source.inputStream.asInput.byteArray
    assertEquals(source,new String(byteArray,codec.charSet))
    val chars = source.inputStream.asInput.chars
    assertEquals(source, chars.mkString)
    val string = source.inputStream.asInput.slurpString
    assertEquals(source, string)
  }
}
