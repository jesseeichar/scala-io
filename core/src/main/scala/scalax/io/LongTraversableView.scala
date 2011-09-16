/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io
import scala.collection.generic.CanBuildFrom
import scala.collection.TraversableView.NoBuilder
import scala.collection.TraversableView


/**
 * The view object associated with LongTraversable.  If you are nor familiar with the pattern essentially a view allows
 * the data to be loaded lazily and only as necessary.  So you can perform slices and maps without loading any data.
 * Only when the data is pulled will it actually be loaded.
 *
 * The behaviour is the essentially the same as for
 * [[http://www.scala-lang.org/api/current/scala/collection/TraversableView.html]] except adding the LongTraversable
 * methods
 */
trait LongTraversableView[+A, +Coll] extends LongTraversableViewLike[A, Coll, LongTraversableView[A, Coll]] {
  override def toString() = "LongTraversableView(...)"
}

/**
 * Defines the required canBuildFrom and Type definitions.  These are required by the collections framework.
 *
 * Probably not interesting API
 */
object LongTraversableView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LongTraversableView[A, LongTraversable[_]]] =
    new CanBuildFrom[Coll, A, LongTraversableView[A, LongTraversable[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}
