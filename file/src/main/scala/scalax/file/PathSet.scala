package scalax.file
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.generic.GenericCompanion
import scala.collection.mutable.Builder
import scalax.io.AbstractLazyIteratorBasedBuilder
import scala.collection.generic.CanBuildFrom
import scalax.io.CompositeIterable
import scala.collection.generic.TraversableFactory


/**
 * An iterable that permits iterating over a directory tree starting at a root Path.  The
 * PathSet is an example of a non-strict collection which means methods like collect, filter, map
 * do not trigger a traversal of the set.  To overcome this restriction one can all force to load
 * the entire set into memory.
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
trait PathSet[+T] extends Iterable[T]
  with PathSetLike[T,PathSet[T]] {

  protected[this] override def newBuilder = PathSet.newBuilder

}

/**
 * This class is not interesting from an API point of view.  It is simply required by the scala collections framework.
 *
 * See the [[scalax.io.PathSet]] class for the truly interesting aspects
 */
object PathSet {
  def pathSetCanBuildFrom[A] = new CanBuildFrom[PathSet[A], A, PathSet[A]] {
    def apply(from: PathSet[A]) = newBuilder
    def apply() = newBuilder
  }
  implicit def canBuildFrom[A]: CanBuildFrom[PathSet[A], A, PathSet[A]] = pathSetCanBuildFrom
  // TODO consider a correct implementation
  def newBuilder[A]: Builder[A, PathSet[A]] =
    new AbstractLazyIteratorBasedBuilder[A, PathSet[A]] {
      override def result() = {
        val iterable = new CompositeIterable[A](builderIterators)
        new IterablePathSet[A](iterable.iterator)
      }
    }

}
