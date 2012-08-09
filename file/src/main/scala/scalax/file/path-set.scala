/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.file
import java.nio.channels.ByteChannel
import collection.{Iterator, IterableView}
import annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scalax.io.AbstractLazyIteratorBasedBuilder
import scalax.io.{CloseableIterator, CloseableIteratorOps}

object PathFinder {
  def empty = new BasicPathSet[Nothing](Nil,PathMatcher.All, -1,false, (_:PathMatcher[Nothing],_:Nothing) => CloseableIterator.empty)
}
trait PathFinder[+T] {
  /**The union of the paths found by this <code>PathSet</code> with the paths found by 'paths'.
   * Note that if the same element is added twice it will be present twice in the PathFinder
   * (in most implementations).  Consider: (Path("a") +++ Path("a")).iterator.  the iterator
   * will return Path("a") twice. */
  def +++[U >: T](includes: PathFinder[U]): PathFinder[U]

  /**Excludes all paths from <code>excludes</code> from the paths selected by this <code>PathSet</code>.*/
  def ---[U >: T](excludes: PathFinder[U]): PathFinder[T]

  /**Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * descendants of paths selected by this finder.
   */
  def **[F](filter: F)(implicit factory:PathMatcherFactory[F]): PathFinder[T]

  def *** : PathFinder[T]

  /**Constructs a new finder that selects all paths with a name that matches <code>filter</code> and are
   * immediate children of paths selected by this finder.
   */
  def *[F](filter: F)(implicit factory:PathMatcherFactory[F]): PathFinder[T]

  /**Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
   * of paths selected by this finder.
   */
  def /(literal: String): PathFinder[T]

  /**Constructs a new finder that selects all paths with name <code>literal</code> that are immediate children
   * of paths selected by this finder.
   */
  def \(literal: String) : PathFinder[T]

  def iterator:CloseableIterator[T]
  /*
   * Makes the paths selected by this finder into base directories.
   */
//  def asBase: PathFinder[T]
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
final class BasicPathSet[+T <: Path](srcFiles: Traversable[T],
                               pathFilter : PathMatcher[T],
                               depth:Int,
                               self:Boolean,
                               children : (PathMatcher[T],T) => CloseableIterator[T])
  extends PathSet[T] {

  def this (parent : T,
            pathFilter : PathMatcher[T],
            depth:Int,
            self:Boolean,
            children : (PathMatcher[T],T) => CloseableIterator[T]) = this(List(parent), pathFilter, depth, self, children)

  override def filter(f: T => Boolean) = {
    if(pathFilter == PathMatcher.All) {
      val newFilter = new PathMatcher.FunctionMatcher(f)
      new BasicPathSet[T](srcFiles,newFilter,depth,self,children)
    } else {
      super.filter(f)
    }
  }
  override def collect [B, That] (pf: PartialFunction[T, B])(implicit bf: CanBuildFrom[PathSet[T], B, That]): That = {
    filter(pf.isDefinedAt).collect(pf)
  }

  def **[F](filter: F)(implicit factory:PathMatcherFactory[F]): PathSet[T] = {
    val nextFilter = factory(filter)
      new BasicPathSet(this, nextFilter, -1, false, children)
  }

    def *[F](filter: F)(implicit factory:PathMatcherFactory[F]): PathSet[T] = {
      val nextFilter = factory(filter)
      new BasicPathSet(this, nextFilter, 1, false, children)
    }
    def *** = ** (PathMatcher.All)

    def / (literal: String): PathSet[T] = new BasicPathSet(this, new PathMatcher.NameIs(literal), 1, false, children)

    /**The union of the paths found by this <code>PathSet</code> with the paths found by 'paths'.*/
    def +++[U >: T](includes: PathFinder[U]): PathSet[U] = new IterablePathSet[U](iterator).+++(includes)

    /**Excludes all paths from <code>excludePaths</code> from the paths selected by this <code>PathSet</code>.*/
    def ---[U >: T](excludes: PathFinder[U]): PathSet[T] = new IterablePathSet[T](iterator).---(excludes)

  override def iterator: CloseableIterator[T] = new CloseableIterator[T] {
    private[this] val roots = srcFiles.toSet
    private[this] val toVisit = if(self) new PathsToVisit(roots.toIterator) else new PathsToVisit(roots.flatMap {p => children(pathFilter,p)}.toIterator)
    private[this] var nextElem : Option[T] = None

    private[this] def root(p:T) = p.parents.find(p => roots.exists(_.path == p.path))

    private[this] def currentDepth(p:Path, root:Option[Path]) = {
      val basicDepth = root.map {r =>
        r.relativize(p).segments.size
        } getOrElse Int.MaxValue
      if(self) basicDepth - 1 else basicDepth
    }
    override def doClose = toVisit.close()
    override def hasNext() = if(nextElem.isDefined) true
                    else {
                      nextElem = loadNext()
                      nextElem.isDefined
                    }

    @tailrec
    private[this] def loadNext() : Option[T] = {
      if(toVisit.isEmpty) None
      else if(toVisit.head.isDirectory) {
        val path = toVisit.next()
        if(depth < 0) {
          toVisit.prepend(children(pathFilter,path))
        } else {
          val currDepth = currentDepth(path, root(path))
          if( depth > currDepth)
            toVisit.prepend(children(pathFilter,path))
        }
        if (pathFilter(path)) Some(path) else loadNext
      }else {
        val file = toVisit.next()
        if (pathFilter(file)) Some(file) else loadNext
      }
    }

    override def next() = {
      def error() = throw new NoSuchElementException("There are no more children in this stream")
      if(!hasNext) error()
      val t = nextElem
      nextElem = None

      t match {
        case None => error()
        case Some(p) => p
      }
    }

    override def toString = "PathSetIterator("+(roots map (_.toString) mkString ",")+")"
  }

  override lazy val toString = getClass().getSimpleName+"(Roots:"+srcFiles+")"
}

private class PathsToVisit[T <: Path](startingIter:Iterator[T]) {
  private[this] var curr = startingIter.buffered
  private[this] var iterators:List[Iterator[T]] = Nil
  def head = curr.head
  @inline
  final def isEmpty = !hasNext
  @tailrec
  final def hasNext:Boolean =
    if(curr.hasNext) true
    else if(iterators.isEmpty) false
    else {
      curr = iterators.head.buffered
      iterators = iterators.tail
      hasNext
  }

  final def next() = curr.next()

  final def prepend(iter:CloseableIterator[T]) = {
    val tmp = curr
    curr = iter.buffered
    if(tmp.hasNext)
      iterators = tmp :: iterators
  }
  
  final def close() = {
    var errors = List[Throwable]()
   iterators.foreach{_ match {
	     case ci: CloseableIterator[_] => 
	       try ci.close catch {case e: Throwable => errors ::= e}
	     case it => ()
	   }
   }
   iterators = null
   curr = null
   errors
  }
}


private class IterablePathSet[T](iter: => CloseableIterator[T]) extends PathSet[T] {
  def iterator = iter
  def mapping[U >: T](f: PathFinder[T] => PathFinder[_]):PathSet[U] = new IterablePathSet(CloseableIteratorOps(iter).flatMap {
    case pf:PathFinder[T] => f(pf).asInstanceOf[PathFinder[U]].iterator
    case o:U => CloseableIterator.empty
  })
  def /(literal: String): PathSet[T] = mapping {
    case p : Path if !(p / literal exists) => PathFinder.empty
    case other => other / literal
  }

  def *[F](filter: F)(implicit factory: PathMatcherFactory[F]): PathSet[T] = mapping(_ * factory(filter))

  def ***  = mapping{_ ***}

  def **[F](filter: F)(implicit factory: PathMatcherFactory[F]): PathSet[T] = mapping{_ ** factory(filter)}

  def ---[U >: T](excludes: PathFinder[U]): PathSet[T] = {
    val excludeSet = excludes.iterator.toSet
    new IterablePathSet(CloseableIteratorOps(iter) filterNot (excludeSet.contains))
  }

  def +++[U >: T](includes: PathFinder[U]): PathSet[U] = new IterablePathSet[U](CloseableIteratorOps(iter) ++ includes.iterator)
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
