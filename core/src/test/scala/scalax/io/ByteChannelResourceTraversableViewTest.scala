/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.ByteArrayInputStream
import java.nio.channels.Channels
import java.nio.ByteBuffer
import scalaio.test.AbstractInputTests
import scalaio.test.stream.InputTest
import collection.mutable.ArrayBuffer

class ByteChannelResourceTraversableViewTest extends ResourceTraversableViewTest {
            /*
  override def traversable[U, A](tsize: Int,
                                 callback: (Int) => U,
                                 dataFunc: (Int) => Traversable[Int],
                                 conv: (Int) => A):LongTraversable[A] = {
    def channel = Channels.newChannel(new ByteArrayInputStream(dataFunc(tsize) map {_.toByte} toArray))
    val toInt = (bb:ByteBuffer) => JavaConversions.byteBufferToTraversable(bb).map(byte => conv(byte.toInt)) : Traversable[Int]

    ResourceTraversable.byteChannelBased(Resource.fromByteChannel(channel), _conv=toInt).map{i => callback(i); i}
  }       */
}

class ByteChannelResourceInputTest extends InputTest {
  def resource(sep:String) =
    Resource.fromReadableByteChannel(Channels.newChannel(stringBasedStream(sep)))
  protected override def input(t: Type):Input = t match {
    case t@TextNewLine => resource(t.sep)
    case t@TextPair => resource(t.sep)
    case t@TextCarriageReturn => resource(t.sep)
    case TextCustom(sep) => resource(sep)
    case TextCustomData(sep, data) => Resource.fromReadableByteChannel(
      Channels.newChannel(new ByteArrayInputStream(data.getBytes(Codec.UTF8.charSet)))
    )
    case Image =>
      Resource.fromReadableByteChannel(
        Channels.newChannel(scalaio.test.Constants.IMAGE.openStream())
      )
  }

}

class SeekableByteChannelResourceInputTest extends InputTest {
  
  def wrap(data:Array[Byte]) = Resource.fromSeekableByteChannel(
    new ArrayBufferSeekableChannel(ArrayBuffer.apply(data:_*),StandardOpenOption.ReadWrite:_*)((),())
  )
  protected override def input(t: Type):Input = t match {
    case t@TextNewLine => wrap(text(t.sep))
    case t@TextPair => wrap(text(t.sep))
    case t@TextCarriageReturn => wrap(text(t.sep))
    case TextCustom(sep) => wrap(text(sep))
    case TextCustomData(sep, data) => wrap(data.getBytes(Codec.UTF8.charSet))
    case Image =>
      val bytes = Resource.fromInputStream(scalaio.test.Constants.IMAGE.openStream()).byteArray
      wrap(bytes)
  }
  override def sizeIsDefined = true
}
