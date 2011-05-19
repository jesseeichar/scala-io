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
  protected override def input(t: Type) = t match {
    case t@TextNewLine => Resource.fromReadableByteChannel(Channels.newChannel(text(t.sep)))
    case t@TextPair => Resource.fromReadableByteChannel(Channels.newChannel(text(t.sep)))
    case t@TextCarriageReturn => Resource.fromReadableByteChannel(Channels.newChannel(text(t.sep)))
    case TextCustom(sep) => Resource.fromReadableByteChannel(Channels.newChannel(text(sep)))
    case TextCustomData(sep, data) => Resource.fromReadableByteChannel(
      Channels.newChannel(new ByteArrayInputStream(data.getBytes(Codec.UTF8.charSet)))
    )
    case Image => Resource.fromReadableByteChannel(Channels.newChannel(scalaio.test.Constants.IMAGE.openStream()))
  }
}

class SeekableByteChannelResourceInputTest extends InputTest {
  protected override def input(t: Type) = t match {
    case t@TextNewLine => Resource.fromReadableByteChannel(Channels.newChannel(text(t.sep)))
    case t@TextPair => Resource.fromReadableByteChannel(Channels.newChannel(text(t.sep)))
    case t@TextCarriageReturn => Resource.fromReadableByteChannel(Channels.newChannel(text(t.sep)))
    case TextCustom(sep) => Resource.fromReadableByteChannel(Channels.newChannel(text(sep)))
    case TextCustomData(sep, data) => Resource.fromReadableByteChannel(
      Channels.newChannel(new ByteArrayInputStream(data.getBytes(Codec.UTF8.charSet)))
    )
    case Image => Resource.fromSeekableByteChannel(Channels.newChannel(scalaio.test.Constants.IMAGE.openStream()))
  }
}
