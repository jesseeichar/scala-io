/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.Closeable

/**
 * An iterator for iterating over the contents of a directory
 * <p>
 * On some FileSystems directories must be opened and closed, therefore
 * DirectoryStream must also be closed after use.
 * </p>
 */
trait DirectoryStream[T] extends Iterator[T] with Closeable

trait SecureDirectoryStream[T] extends DirectoryStream[T]
