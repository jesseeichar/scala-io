/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io
import java.io.Closeable
import java.nio.channels.ByteChannel
import collection.{Iterator, IterableView}

trait PathFinder[+T] {
  /**The union of the paths found by this <code>PathSet</code> with the paths found by 'paths'.*/
  def +++[U >: T](includes: PathFinder[U]): PathFinder[U]

  /**Excludes all paths from <code>excludes</code> from the paths selected by this <code>PathSet</code>.*/
  def ---[U >: T](excludes: PathFinder[U]): PathFinder[U]

  /**Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * descendants of paths selected by this finder.
   */
  def **[U >: T, F](filter: F)(implicit factory:PathMatcherFactory[F]): PathFinder[U]

  def ***[U >: T] : PathFinder[U]

  /**Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * immediate children of paths selected by this finder.
   */
  def *[U >: T, F](filter: F)(implicit factory:PathMatcherFactory[F]): PathFinder[U]

  /**Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
   * of paths selected by this finder.
   */
  def /(literal: String): PathFinder[T]

  /**Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
   * of paths selected by this finder.
   */
  def \(literal: String) : PathFinder[T]

  /**
   * Makes the paths selected by this finder into base directories.
   */
  def toBase: PathFinder[T]

  def iterator : Iterator[T]
}

/**
 * An iterable that permits iterating over a directory tree starting at a root Path.  The
 * PathSet is an example of a non-strict collection.
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
trait PathSet[+T] extends Iterable[T] with PathFinder[T] {
  /**The union of the paths found by this <code>PathSet</code> with the paths found by 'paths'.*/
  def +++[U >: T](includes: PathFinder[U]): PathSet[U]

  /**Excludes all paths from <code>excludes</code> from the paths selected by this <code>PathSet</code>.*/
  def ---[U >: T](excludes: PathFinder[U]): PathSet[U]

  /**Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * descendants of paths selected by this finder.
   */
  def **[U >: T, F](filter: F)(implicit factory:PathMatcherFactory[F]): PathSet[U]

  def ***[U >: T] : PathSet[U]

  /**Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * immediate children of paths selected by this finder.
   */
  def *[U >: T, F](filter: F)(implicit factory:PathMatcherFactory[F]): PathSet[U]

  /**Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
   * of paths selected by this finder.
   */
  def /(literal: String): PathSet[T]

  /**Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
   * of paths selected by this finder.
   */
  def \(literal: String) = /(literal)

  /**
   * Makes the paths selected by this finder into base directories.
   */
  def toBase: PathSet[T] = null.asInstanceOf[PathSet[T]]
}


/**
 * Directory stream implementation to assist in implementing 
 * DirectoryStreams that are based on Paths.
 * 
 * @parent 
 *          the path from where the PathSet is derived.  The first entry
 *          of the PathSet is the first child of the parent path
 * @pathFilter
 *          A filter to restrict the contents of the PathSet
 * @depth
 *          The depth that the stream should traverse
 * @children
 *          A function to use for retrieving the children of a particular path
 *          This method is used to retrieve the children of each directory
 */
final class BasicPathSet[+T <: Path](srcFiles: Iterable[T],
                               pathFilter : PathMatcher,
                               depth:Int,
                               self:Boolean,
                               children : T => List[T]) extends PathSet[T] {

  def this (parent : T,
            pathFilter : PathMatcher,
            depth:Int,
            self:Boolean,
            children : T => List[T]) = this(List(parent), pathFilter, depth, self, children)

    def **[U >: T, F](filter: F)(implicit factory:PathMatcherFactory[F]): PathSet[U] = {
      val nextFilter = factory(filter)
      new BasicPathSet(this, nextFilter, -1, false, children)
    }

    def *[U >: T, F](filter: F)(implicit factory:PathMatcherFactory[F]): PathSet[U] = {
      val nextFilter = factory(filter)
      new BasicPathSet(this, nextFilter, 1, false, children)
    }
    def ***[U >: T] : PathSet[U] = ** (PathMatcher.All)

    def / (literal: String): PathSet[T] = new BasicPathSet(this, new PathMatcher.NameIs(literal), 1, false, children)

    /**The union of the paths found by this <code>PathSet</code> with the paths found by 'paths'.*/
    def +++[U >: T](includes: PathFinder[U]): PathSet[U] = new AdditivePathSet[U](this, includes)

    /**Excludes all paths from <code>excludePaths</code> from the paths selected by this <code>PathSet</code>.*/
    def ---[U >: T](excludes: PathFinder[U]): PathSet[U] = new SubtractivePathSet[U](this,excludes)

  def iterator: Iterator[T] = new Iterator[T] {
    val roots = srcFiles.toSet
    var toVisit = if(self) roots.toList else {roots.toList flatMap children}
    var nextElem : Option[T] = None

    def root(p:Path) = p.parents.foldRight (None:Option[Path]){
      case (_, found @ Some(_)) => found
      case (prevParent, _) => roots.find{_ == prevParent}
    }
    
    def currentDepth(p:Path, root:Option[Path]) = {
      val basicDepth = root.map {r => p.relativize(r).segments.size} getOrElse Int.MaxValue
      if(self) basicDepth - 1 else basicDepth 
    }

    def hasNext() = if(nextElem.nonEmpty) true
                    else {
                      nextElem = loadNext()
                      nextElem.nonEmpty
                    }
    
    def loadNext() : Option[T] = {
      toVisit match {
        case Nil => None
        case path :: _ if path.isDirectory =>

          if(depth < 0 || depth > currentDepth(path, root(path)))
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


private trait SourceBasedPathSet[+T] {
  self:PathSet[T] =>

  def /(literal: String): PathSet[T] = mapSources{_ / literal}

  def *[U >: T, F](filter: F)(implicit factory: PathMatcherFactory[F]): PathSet[U] = mapSources{_ * factory(filter)}

  def ***[U >: T]: PathSet[U] = mapSources{_ ***}

  def **[U >: T, F](filter: F)(implicit factory: PathMatcherFactory[F]): PathSet[U] = mapSources{_ ** factory(filter)}

  def ---[U >: T](excludes: PathFinder[U]): PathSet[U] = new SubtractivePathSet[U](this,excludes)

  def +++[U >: T](includes: PathFinder[U]): PathSet[U] = new AdditivePathSet(this, includes)

  def mapSources[U >: T](f: PathFinder[U] => PathFinder[U]):PathSet[U]
}

private class AdditivePathSet[+T](original:PathFinder[T], more:PathFinder[T])
        extends PathSet[T] with SourceBasedPathSet[T] {
  def iterator: Iterator[T] = original.iterator ++ more.iterator

  def mapSources[U >: T](f: PathFinder[U] => PathFinder[U]):PathSet[U] = new AdditivePathSet[U](f(original),f(more))
}

private class SubtractivePathSet[+T](original:PathFinder[T], excludes:PathFinder[T])
        extends PathSet[T] with SourceBasedPathSet[T] {
  def iterator: Iterator[T] = {
    val excludeSet = excludes.iterator.toSet
    original.iterator filterNot (excludeSet.contains)
  }
  def mapSources[U >: T](f: PathFinder[U] => PathFinder[U]):PathSet[U] = new SubtractivePathSet[U](f(original),f(excludes))
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
   * Opens the directory identified by the given path, returning a PathSet[SecuredPath] to iterate over the entries in the directory.
   */
  def newDirectoryStream(path:T /*LinkOption... options*/): PathSet[T]
          
}
*/
