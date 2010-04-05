/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.resource.ManagedResourceOperations
import scalax.io.Codec

import java.nio.channels.{
    Channels, ByteChannel
}

/***************************** ByteChannelResource ************************************/
/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class ByteChannelResource[+A <: ByteChannel](opener: => A) extends InputResource[A] with OutputResource[A] with CloseableResource[A] {
    def open() = opener

    def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
    def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
    def reader(implicit sourceCodec: Codec) = Resource.fromReader(Channels.newReader(opener, sourceCodec.charSet.name()))
    def writer(implicit sourceCodec: Codec) = Resource.fromWriter(Channels.newWriter(opener, sourceCodec.charSet.name()))
    def writableByteChannel = Resource.fromWritableByteChannel(opener)
    def readableByteChannel = Resource.fromReadableByteChannel(opener)

    def bytesAsInts:Traversable[Int] = toTraversable {in => StreamIterator(Channels.newInputStream(opener))}
    def chars(implicit codec: Codec): Traversable[Char] = reader(codec).chars
}
