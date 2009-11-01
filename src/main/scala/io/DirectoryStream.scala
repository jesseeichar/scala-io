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
 *
 * TODO ControlException docs
 * TODO ConcurrencyException docs
 * 
 */
trait DirectoryStream[T] extends Iterable[T] {
  /**
   * Iterates over the contents of the directory passing each element to the
   * function.
   * <p>
   * The partial function does not need to be complete, all Path's that do not have matches in the function
   * will be ignored.  For example: <code>contents {case File(p)=>println(p+" is a file")}</code> would match
   * all Files.  To assist in matching Paths see the {@link Extractors} and
   * {@link FileSystem.matcher(String,String)}
   * </p>
   * @param function the function that is used to process each entry in the directory
   *
   * @return nothing
   *
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
  def filterEach (function: PartialFunction[Path,Unit]): Unit

  /**
   * Iterates over the contents of the directory passing each element to the
   * function and returns the result of the computation.
   * <p>
   * The partial function does not need to be complete, all Path's that do not have matches in the function
   * will be ignored.  For example: <code>contents {case File(p)=>println(p+" is a file")}</code> would match
   * all Files.  To assist in matching Paths see the {@link Extractors} and
   * {@link FileSystem.matcher(String,String)}
   * </p>
   *
   * @param initial the value that is passed to the first call of function
   * @param function the function that is used to process each entry in the directory
   *
   * @return The result from the last call to PartialFunction or None if there were no matches
   *
   * @see #filterEach(PartialFunction)
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
  def filterFold[R] (initial:R)(function: PartialFunction[(R, Path),R]): Option[R]
}

/**
 * TODO scaladocs
 */
trait SecureDirectoryStream[T] extends DirectoryStream[T] {
  // TODO methods
}
