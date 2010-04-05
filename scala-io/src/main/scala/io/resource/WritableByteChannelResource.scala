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

import java.io.BufferedOutputStream
import java.nio.channels.{
    Channels, WritableByteChannel
}

/***************************** WritableByteChannelResource ************************************/

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class WritableByteChannelResource[+A <: WritableByteChannel](opener: => A) extends BufferableOutputResource[A, BufferedOutputStream] with CloseableResource[A] {
    def open() = opener

    def buffered = outputStream.buffered    
    def outputStream = Resource.fromOutputStream(Channels.newOutputStream(opener))
    def writer(implicit sourceCodec: Codec) = Resource.fromWriter(Channels.newWriter(opener, sourceCodec.charSet.name()))
    def writableByteChannel = this
}

