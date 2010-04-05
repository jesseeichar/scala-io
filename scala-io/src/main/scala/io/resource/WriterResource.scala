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
    Writer, BufferedWriter
}

/***************************** WriterResource ************************************/
/**
 * A ManagedResource for accessing and using Writers.
 *
 * @see ManagedResource
 */
class WriterResource[+A <: Writer](opener: => A, val sourceCodec:Codec) extends BufferableWriteCharsResource[A, BufferedWriter] with CloseableResource[A] {
    def open() = opener

    def buffered = Resource.fromBufferedWriter(new BufferedWriter(opener))(sourceCodec)

    protected def writer = this
}
