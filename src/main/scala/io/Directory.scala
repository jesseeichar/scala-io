/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io
import scala.resource.ManagedTraversable

abstract class Directory extends ManagedTraversable[DirectoryStream[Path],Path] {
  /**
   * Iterates over the contents of the directory passing each element to the
   * function.
   * <p>
   * This method is non-locking so the contents of the directory may change
   * during the execution of this method.  Files and Directories may be deleted
   * at any time including during the period that the function is executing with
   * a Path to a file.
   * </p><p>
   * For stronger guarantees use the contents method If the filesystem supports it
   * then the contents method will return a {@link ManagedResource} with a {@link SecuredDirectoryStream}
   * </p>
   * <p>
   * The partial function does not need to be complete, all Path's that do not have matches in the function
   * will be ignored.  For example: <code>contents {case File(p)=>println(p+" is a file")}</code> would match
   * all Files.  To assist in matching Paths see the {@link Extractors} and
   * {@link FileSystem.matcher(String,String)}
   * </p>
   * <p>
   * Note: If a PathMatcher is used in PartialFunction this method may be less efficient than
   * {@link Path#directoryStream(Option[PathMatcher], Boolean)} because there is no way that this
   * method can detect the PathMatcher and therefore all content objects must be read from disk and
   * processed by the PartialFunction in order to determine if a match is found.
   * <p>
   * Compared to {@link Path#directoryStream(Option[PathMatcher], Boolean)} which can pass the
   * matcher to the underlying FileSystem and have the filesystem perform the filtering (if the
   * filesystem supports the functionality natively
   * </p>
   * </p>
   * @param function the function that is used to process each entry in the directory
   *
   * @return nothing
   *
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
//  def contents (function: PartialFunction[Path,Unit]): Unit

  /**
   * Iterates over the contents of the directory passing each element to the
   * function and returns the result of the computation.
   * <p>
   * See {@link Path#directoryStream(Function)} for details and restrictions on how the
   * directories are processed.
   * </p>
   *
   * @param initial the value that is passed to the first call of function
   * @param function the function that is used to process each entry in the directory
   *
   * @return The result from the last call to PartialFunction or None if there were no matches
   *
   * @see Path#directoryStream(Function)
   * @see Path.Matching
   * @see FileSystem#matcher(String,String)
   */
//  def foldDirectoryStream[R] (initial:R)(function: PartialFunction[(R, Path),R]): Option[R]

}
