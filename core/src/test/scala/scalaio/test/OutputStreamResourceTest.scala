package scalaio.test

import org.junit.Assert._
import org.junit.{
  Test,
  Before,
  After,
  Rule,
  Ignore
}
import scalax.io.Resource
import java.io.ByteArrayOutputStream
import scalax.io.Codec
import scalax.io.OutputResource
import java.nio.channels.Channels

class OutputStreamResourceTest {
  @Test
  def convert_to_writer_must_respect_codec {
    var out = new ByteArrayOutputStream()
    def outResource = {
      out = new ByteArrayOutputStream()
      Resource.fromOutputStream(out)
    }

    def test(outResource: => OutputResource[_]) = {
    val data = "\u00E0"
        outResource.writer(Codec.UTF8).write(data)
        assertEquals(data, new String(out.toByteArray(), "UTF-8"))

        outResource.writer(Codec.ISO8859).write(data)
        assertEquals(data, new String(out.toByteArray(), Codec.ISO8859.name))
    }
    test {
      out = new ByteArrayOutputStream()
      Resource.fromOutputStream(out)
    }

    test {
      out = new ByteArrayOutputStream()
      Resource.fromOutputStream(out).writableByteChannel
    }

    test {
      out = new ByteArrayOutputStream()
      Resource.fromWritableByteChannel(Channels.newChannel(out))
    }

    test {
        out = new ByteArrayOutputStream()
        Resource.fromWritableByteChannel(Channels.newChannel(out)).outputStream
    }


  }
}
