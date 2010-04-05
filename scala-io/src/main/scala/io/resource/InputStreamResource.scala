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
  InputStream, BufferedInputStream, InputStreamReader
}
import java.nio.channels.Channels

/***************************** InputStreamResource ************************************/
/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource
 */
class InputStreamResource[+A <: InputStream](opener: => A) extends BufferableInputResource[A, BufferedInputStream] with CloseableResource[A] {
    def open() = opener

    def inputStream = this
    def buffered = Resource.fromBufferedInputStream(new BufferedInputStream(opener))
    def reader(implicit sourceCodec: Codec) = Resource.fromReader(new InputStreamReader(opener, sourceCodec.charSet))
    def readableByteChannel = Resource.fromReadableByteChannel(Channels.newChannel(open()))
    def chars(implicit codec: Codec): Traversable[Char] = reader(codec).chars

    def bytesAsInts:Traversable[Int] = toTraversable {in => StreamIterator(in)}
}
