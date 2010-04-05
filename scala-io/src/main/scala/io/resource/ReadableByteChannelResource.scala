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

import java.io.BufferedInputStream
import java.nio.channels.{
    Channels, ReadableByteChannel
}

/***************************** ReadableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel](opener: => A) extends BufferableInputResource[A, BufferedInputStream] with CloseableResource[A] {
    def open() = opener
    
    def buffered = inputStream.buffered
    def inputStream = Resource.fromInputStream(Channels.newInputStream(opener))
    def reader(implicit sourceCodec: Codec) = Resource.fromReader(Channels.newReader(opener, sourceCodec.charSet.name()))
    def readableByteChannel = this
    def bytesAsInts:Traversable[Int] = toTraversable {in => StreamIterator(Channels.newInputStream(opener))}
    def chars(implicit codec: Codec): Traversable[Char] = reader(codec).chars
}
