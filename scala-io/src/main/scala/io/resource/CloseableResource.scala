/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.resource.ManagedResourceOperations

import java.io.Closeable

/**
 * A Resource that can be used to do IO.  It wraps objects from the java.io package
 *
 * @param R
 *          The type of the resource that will be managed by the ManagedResource
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
trait CloseableResource[+R <: Closeable] {
    self : ManagedResourceOperations[R] =>
    /**
    * Creates a new InputStream (provided the code block used to create the resource is
    * re-usable).  This method should only be used with care in cases when Automatic
    * Resource Management cannot be used because the
    * {@link InputStream} must be closed manually.
    * <p>
    * This is public only to permit interoperability with certain Java APIs.
    * A better pattern of use should be:
    * <code>
    * resource.acquireFor {
    *   // call java API
    * }
    * </code>
    * or
    * <code>
    * val calculatedResult = resource.acquireAndGet {
    *   // cal java API that returns a result
    * }
    * </code>
    */
    def open(): R
    
    def acquireFor[B](f : R => B) : Either[List[Throwable], B] = {

        val resource = open()

        var exceptions = List[Throwable]()
        val result = try {
            Some(f(resource))
        } catch {
            case e => 
                exceptions ::= e 
                None
        } finally {
            try {
                resource.close()
            } catch {
                case e => 
                    exceptions ::= e
            }
        }
        
        result match {
            case Some(r) => Right(r)
            case None => Left(exceptions)
        }
    }

}

