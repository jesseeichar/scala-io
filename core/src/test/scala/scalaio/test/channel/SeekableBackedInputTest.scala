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
  override protected def textResource(sep: String, closeFunction: () => Unit): Input =
    		  construct(text(sep), closeFunction)

  override protected def customDataResource(data: String, closeFunction: () => Unit): Input = 
		  construct(data.getBytes(Codec.UTF8.charSet), closeFunction)

  override protected def imageResource(closeFunction: () => Unit): Input = {
    construct(Resource.fromInputStream(Constants.IMAGE.openStream()).bytes, closeFunction)
  }
  
  def construct(bytes:Traversable[Byte], closeFunction: () => Unit) = {
    val buffer = new ArrayBuffer[Byte]() ++ bytes
    val channel = new ArrayBufferSeekableChannel(buffer)(closeAction = _ => closeFunction())
    Resource.fromSeekableByteChannel(channel)
  }

}