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

import java.io.{
    OutputStream, BufferedOutputStream, OutputStreamWriter
}
import java.nio.channels.Channels

/***************************** OutputStreamResource ************************************/

/**
 * A ManagedResource for accessing and using OutputStreams.
 *
 * @see ManagedResource
 */
class OutputStreamResource[+A <: OutputStream](opener: => A) extends BufferableOutputResource[A, BufferedOutputStream] with CloseableResource[A] {
    def open() = opener

    def outputStream = this
    def buffered = Resource.fromBufferedOutputStream(new BufferedOutputStream(opener))
    def writer(implicit sourceCodec: Codec) = Resource.fromWriter(new OutputStreamWriter(opener, sourceCodec.charSet))
    def writableByteChannel = Resource.fromWritableByteChannel(Channels.newChannel(open()))
}
