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
import scala.collection.mutable.{ Builder, ListBuffer }
import scala.collection.mutable.Queue

/**
 * A traversable for use on very large datasets which cannot be indexed with Ints but instead
 * require Longs for indexing.  This trait adds methods for accessing the extra portions
 * of the dataset.
 */
trait LongTraversable[@specialized(Byte,Char) +A] extends Traversable[A]
  with GenericTraversableTemplate[A, LongTraversable]
  with LongTraversableLike[A, LongTraversable[A]] {
  self =>
  override def companion: GenericCompanion[LongTraversable] = LongTraversable

  protected[this] override def newBuilder = LongTraversable.newBuilder
  def force: LongTraversable[A] = {
    val b = new ListBuffer[A] mapResult (x => new LongTraversableImpl[A](x))
    withIterator{b ++= _} 
    b.result()
  }
  def open = new OpenedLongTraversable(this)

  override def toString() = "LongTraversable(...)"
}

/**
 * This class is not interesting from an API point of view.  It is simply required by the scala collections framework.
 *
 * See the [[scalax.io.LongTraversable]] class for the truly interesting aspects
 */
object LongTraversable extends TraversableFactory[LongTraversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LongTraversable[A]] = new GenericCanBuildFrom[A]
  // TODO consider a correct implementation
  def newBuilder[A]: LongTraversableBuilder[A, LongTraversable[A]] =
    new AbstractLazyIteratorBasedBuilder[A, LongTraversable[A]] with LongTraversableBuilder[A, LongTraversable[A]] {
      override def result() = new CompositeIterable[A](builderIterators) with LongTraversable[A]
      def fromIterator(iter: => CloseableIterator[A]): LongTraversable[A] = new LongTraversable[A] {
        def iterator = iter
      }

    }

  def apply[A](iteratorImpl: => CloseableIterator[A], toStringImpl: String = "LongTraversable(...)") = new LongTraversable[A] {
    protected[io] def iterator: CloseableIterator[A] = iteratorImpl
    override def toString() = toStringImpl
  }
}

private class LongTraversableImpl[A](contained: Traversable[A]) extends LongTraversable[A] {
  protected[io] def iterator: CloseableIterator[A] = contained match {
    case c: LongTraversable[A] => c.iterator
    case _ => CloseableIterator(contained.toIterator)
  }
}
