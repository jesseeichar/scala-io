/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
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
import java.io.InputStream
import java.nio.channels.Channels

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
  def convert_to_reader_should_respect_codec {
    val data = "\u00E0"
    def test(inResource:InputResource[_]) {
      assertEquals(data, inResource.reader(codec).string)
      assertEquals(2, inResource.reader(Codec.ISO8859).string.size)
      assertEquals(data, inResource.readableByteChannel.reader(codec).string)
      assertEquals(2, inResource.readableByteChannel.reader(Codec.ISO8859).string.size)

      assertEquals(data, inResource.chars(codec).mkString)
      assertEquals(2, inResource.chars(Codec.ISO8859).mkString.size)
      assertEquals(data, inResource.readableByteChannel.chars(codec).mkString)
      assertEquals(2, inResource.readableByteChannel.chars(Codec.ISO8859).mkString.size)
    }

    test(Resource.fromInputStream(data.inputStream))
    test(Resource.fromReadableByteChannel(Channels.newChannel(data.inputStream(codec))))
    test(Resource.fromReadableByteChannel(Channels.newChannel(data.inputStream(codec))).inputStream)
  }

  @Test
  def bytesAsInts_and_bytes_should_have_same_elements_after_simple_map = {
    assertEquals(resource.bytes.map(_.toInt).toList, resource.bytesAsInts.toList)
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
    import JavaConverters._
    class InputStreamCloseCounter(in:InputStream) extends InputStream {
      var closes = 0;
      def read = in.read

      override def close = {
        closes += 1
        in.close
      }
    }
    val in = new InputStreamCloseCounter(source.inputStream)
    val byteArray = in.asInput.byteArray
    assertEquals(1,in.closes)
    assertEquals(source,new String(byteArray,codec.charSet))
    assertEquals(source,new String(byteArray,codec.charSet))

    val in2 = new InputStreamCloseCounter(source.inputStream)
    val chars = in2.asInput.chars.mkString
    assertEquals(1,in2.closes)
    assertEquals(source, chars)

    val in3 = new InputStreamCloseCounter(source.inputStream)
    val string = in3.asInput.string
    assertEquals(1,in3.closes)
    assertEquals(source, string)
  }
}
