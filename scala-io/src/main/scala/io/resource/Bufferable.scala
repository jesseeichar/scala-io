/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.resource.ManagedResourceOperations

/**
 * An Object that has an associated Buffered object. For example InputStream
 * has BufferedInputStream
 *
 * @param C
 *          The resource type
 * @param B
 *          they type of the buffered resource 
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait Bufferable[R <: ManagedResourceOperations[_]] {
    /**
    * Obtain the buffered version of this object.
    *
    * @return the buffered version of this object
    */
    def buffered: R
}

trait BufferableInputResource[+C, B] extends InputResource[C] with Bufferable[InputResource[B]]
trait BufferableOutputResource[+C, B] extends OutputResource[C] with Bufferable[OutputResource[B]]
trait BufferableReadCharsResource[+C, B] extends ReadCharsResource[C] with Bufferable[ReadCharsResource[B]]
trait BufferableWriteCharsResource[+C, B] extends WriteCharsResource[C] with Bufferable[WriteCharsResource[B]]

