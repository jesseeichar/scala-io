/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scalaio.test.AbstractInputTests._
import java.io.ByteArrayInputStream
import java.nio.channels.Channels
import java.nio.ByteBuffer
import scalaio.test.stream.InputTest
import collection.mutable.ArrayBuffer
import java.io.IOException

class ByteChannelResourceInputTest extends InputTest {
  def resource(sep:String, openFunction: () => Unit, closeFunction: () => Unit) =
    Resource.fromReadableByteChannel(Channels.newChannel(stringBasedStream(sep, openFunction, closeFunction)))
  protected override def input(t: Type, openFunction: () => Unit, closeFunction: () => Unit):Input = t match {
    case t@TextNewLine => resource(t.sep, openFunction, closeFunction)
    case t@TextPair => resource(t.sep, openFunction, closeFunction)
    case t@TextCarriageReturn => resource(t.sep, openFunction, closeFunction)
    case TextCustom(sep) => resource(sep, openFunction, closeFunction)
    case TextCustomData(sep, data) => Resource.fromReadableByteChannel{
      val stream = new ByteArrayInputStream(data.getBytes(Codec.UTF8.charSet)){
        openFunction()
        override def close() = closeFunction()
      }
      Channels.newChannel(stream)}

    case ErrorOnRead =>
      Resource.fromReadableByteChannel(Channels.newChannel(ErrorOnRead.errorInputStream))
    case ErrorOnClose =>
      Resource.fromReadableByteChannel(Channels.newChannel(ErrorOnClose.errorInputStream))
    case Image =>
      Resource.fromReadableByteChannel({
        openFunction()
        Channels.newChannel(scalaio.test.Constants.IMAGE.openStream(closeFunction))
      })
  }

}

class SeekableByteChannelResourceInputTest extends InputTest {

  def wrap(data:Array[Byte], openFunction: () => Unit, closeFunction: () => Unit) = Resource.fromSeekableByteChannel({
    openFunction()
    new ArrayBufferSeekableChannel(ArrayBuffer.apply(data:_*),StandardOpenOption.ReadWrite:_*)(closeAction = _ => closeFunction())
  })
  protected override def input(t: Type, openFunction: () => Unit, closeFunction: () => Unit):Input = t match {
    case t@TextNewLine => wrap(text(t.sep), openFunction, closeFunction)
    case t@TextPair => wrap(text(t.sep), openFunction, closeFunction)
    case t@TextCarriageReturn => wrap(text(t.sep), openFunction, closeFunction)
    case TextCustom(sep) => wrap(text(sep), openFunction, closeFunction)
    case TextCustomData(sep, data) => wrap(data.getBytes(Codec.UTF8.charSet), openFunction, closeFunction)
    case ErrorOnRead =>
      Resource.fromSeekableByteChannel(
        new ArrayBufferSeekableChannel(ArrayBuffer[Byte](),StandardOpenOption.ReadWrite:_*)() {
           override def read(dst : ByteBuffer) = throw new IOException("boom")
      })
    case ErrorOnClose =>
      val bytes = Resource.fromInputStream(scalaio.test.Constants.IMAGE.openStream()).byteArray
      Resource.fromSeekableByteChannel(
        new ArrayBufferSeekableChannel(ArrayBuffer[Byte](bytes:_*),StandardOpenOption.ReadWrite:_*)() {
           override def close() =
             throw new IOException("boom")
      })
    case Image =>
      val bytes = Resource.fromInputStream(scalaio.test.Constants.IMAGE.openStream()).byteArray
      wrap(bytes, openFunction, closeFunction)
  }
  override def sizeIsDefined = true
}
