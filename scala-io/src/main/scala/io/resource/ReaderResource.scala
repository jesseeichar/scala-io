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
    Reader, BufferedReader
}

/***************************** ReaderResource ************************************/

/**
 * A ManagedResource for accessing and using Readers.
 *
 * @see ManagedResource
 */
class ReaderResource[+A <: Reader](opener: => A, val sourceCodec:Codec) extends BufferableReadCharsResource[A, BufferedReader] with CloseableResource[A] {
    def open() = opener

    def buffered = Resource.fromBufferedReader(new BufferedReader(opener))(sourceCodec)

    override def chars : Traversable[Char] = toTraversable {reader => StreamIterator(reader)}
}
