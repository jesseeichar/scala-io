/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io
import scala.collection.IterableView

import java.io.Closeable
import java.nio.channels.ByteChannel
/**
 * An iterable that permits iterating over a directory tree starting at a root Path.  The
 * DirectoryStream is an example of a non-strict collection. 
 * 
 * <p>
 * When a method is called the root Path is checked to determine if it is a Directory.  If not
 * a NotDirectoryException is thrown.
 * </p>
 * <p>
 * If an IOException is encountered while iterating a ConcurrentModificationException is thrown with
 * case IOException
 * </p>
 * @see NotDirectoryException
 * 
 * @author  Jesse Eichar
 * @since   1.0
 */
abstract class DirectoryStream[+T] extends Iterable[T]

/*
 * Will uncomment this for the jdk7 version
trait SecuredPath[T] {
  val path: T
  /**
   * Deletes a directory.
   */
  def deleteDirectory(path:T): Unit
  /**
   * Deletes a file.
   */
  def deleteFile(path:T): Unit
  /**
   * Move a file from this directory to another directory.
   *
   * @TODO verify that this method is possible
   */
  def move(srcpath:T, targetdir:SecuredPath[T], targetpath:T): Unit
          
  /*
   * Opens or creates a file in this directory, returning a seekable byte channel to access the file.
   */
  def newByteChannel(path:T, options:Set[OpenOption] /*FileAttribute<?>... attrs*/): ByteChannel 
     
  /**
   * Opens the directory identified by the given path, returning a DirectoryStream[SecuredPath] to iterate over the entries in the directory.
   */
  def newDirectoryStream(path:T /*LinkOption... options*/): DirectoryStream[T] 
          
}
*/
