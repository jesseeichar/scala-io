package scalaio.test.channel

import java.nio.channels.Channels
import scalax.io.ArrayBufferSeekableChannel
import scala.collection.mutable.ArrayBuffer
import scalax.io.Resource
import scalax.io.Input
import scalax.io.Codec
import scalaio.test.Constants
import scalax.io.support.FileUtils

class SeekableBackedInputTest extends InputTest {

  override def sizeIsDefined = true
  override protected def textResource(sep: String): Input =
    		  construct(text(sep))

  override protected def customDataResource(data: String): Input = 
		  construct(data.getBytes(Codec.UTF8.charSet))

  override protected def imageResource: Input = {
    construct(Resource.fromInputStream(Constants.IMAGE.openStream()).bytes)
  }
  
  def construct(bytes:Traversable[Byte]) = {
    val buffer = new ArrayBuffer[Byte]() ++ bytes
    val channel = new ArrayBufferSeekableChannel(buffer)(_ => (), _ => ())
    Resource.fromSeekableByteChannel(channel)
  }

}