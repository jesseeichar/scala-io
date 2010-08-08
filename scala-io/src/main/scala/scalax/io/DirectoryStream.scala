/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
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

/**
 * Directory stream implementation to assist in implementing 
 * DirectoryStreams that are based on Paths.
 * 
 * @parent 
 *          the path from where the DirectoryStream is derived.  The first entry
 *          of the DirectoryStream is the first child of the parent path
 * @pathFilter
 *          A filter to restrict the contents of the DirectoryStream
 * @depth
 *          The depth that the stream should traverse
 * @children
 *          A function to use for retrieving the children of a particular path
 *          This method is used to retrieve the children of each directory
 */
abstract class AbstractPathDirectoryStream[+T <: Path](parent : T, 
                             pathFilter : Path => Boolean,
                             depth:Int,
                             children : T => List[T]) extends DirectoryStream[T] {
                               
  assert(parent.isDirectory, "parent of a directory stream must be a Directory")
  
  def iterator: Iterator[T] = new Iterator[T] {
    var toVisit = children(parent)
    var nextElem : Option[T] = None
    
    def hasNext() = if(nextElem.nonEmpty) true
                    else {
                      nextElem = loadNext()
                      nextElem.nonEmpty
                    }
    
    def loadNext() : Option[T] = {
      toVisit match {
        case Nil => None
        case path :: _ if path.isDirectory =>
          if(depth < 0 || path.relativize(parent).segments.size < depth)
            toVisit = children(path) ::: toVisit.tail
          else
            toVisit = toVisit.tail
          Some(path).filter(pathFilter).orElse{loadNext}
        case file :: _ => 
          toVisit = toVisit.tail
          Some(file).filter(pathFilter).orElse{loadNext}
      }
    }
    
    def next() = {
      def error() = throw new NoSuchElementException("There are no more children in this stream")
      if(!hasNext) error()
      val t = nextElem
      nextElem = None
      
      t match {
        case None => error()
        case Some(p) => p
      }
    }
  }
}

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
