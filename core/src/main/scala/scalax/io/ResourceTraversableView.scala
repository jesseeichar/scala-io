/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection._
import scala.collection.generic._
import TraversableView.NoBuilder
import java.io.Closeable

/**
 * A LongTraversableView that is uses a Resource as its underpinnings
 */
private[io] trait ResourceTraversableView[A, +Coll] extends ResourceTraversableViewLike[A, Coll, ResourceTraversableView[A, Coll]]

/**
 * The required methods by the Scala collections design
 */
private[io] object ResourceTraversableView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ResourceTraversableView[A, ResourceTraversable[_]]] =
    new CanBuildFrom[Coll, A, ResourceTraversableView[A, ResourceTraversable[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}

