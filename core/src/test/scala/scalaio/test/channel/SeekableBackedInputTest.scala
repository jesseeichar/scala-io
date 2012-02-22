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
  override protected def textResource(sep: String, openFunction: () => Unit, closeFunction: () => Unit): Input =
    		  construct(text(sep), openFunction, closeFunction)

  override protected def customDataResource(data: String, openFunction: () => Unit, closeFunction: () => Unit): Input =
		  construct(data.getBytes(Codec.UTF8.charSet), openFunction, closeFunction)

  override protected def imageResource(openFunction: () => Unit, closeFunction: () => Unit): Input = {
    construct(Resource.fromInputStream(Constants.IMAGE.openStream()).bytes, openFunction, closeFunction)
  }

  def construct(bytes:Traversable[Byte], openFunction: () => Unit, closeFunction: () => Unit) = {
    val buffer = new ArrayBuffer[Byte]() ++ bytes
    def channel = new ArrayBufferSeekableChannel(buffer)(closeAction = _ => closeFunction()) {
      openFunction()
    }
    Resource.fromSeekableByteChannel(channel)
  }

}