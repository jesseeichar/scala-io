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
trait LongTraversable[@specialized(Byte) +A] extends Traversable[A]
  with GenericTraversableTemplate[A, LongTraversable]
  with LongTraversableLike[A, LongTraversable[A]] {
  self =>
  override def companion: GenericCompanion[LongTraversable] = LongTraversable

  protected[this] override def newBuilder = LongTraversable.newBuilder
  def force: LongTraversable[A] = {
    val b = new ListBuffer[A] mapResult (x => new LongTraversableImpl[A](x))
    b ++= iterator
    b.result()
  }

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
  def newBuilder[A]: Builder[A, LongTraversable[A]] = new LongTraversableBuilderImpl[A]

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

private class LongTraversableBuilderImpl[A]()
  extends LongTraversableBuilder[A, LongTraversable[A]] {
  def fromIterator(iter: => CloseableIterator[A]): LongTraversable[A] = new LongTraversable[A] {
    def iterator = iter
  }
  val builderIterators: Queue[() => Iterator[A]] = Queue.empty
  override def clear() = builderIterators.clear()
  override def result() = new CompositeIteratorLongTraversable[A](builderIterators)

  private def addCollector(xs: TraversableOnce[A]) {
    val collector = new Collector
    collector.elements ++= xs
    builderIterators += collector
  }
  override def +=(elem: A): this.type = {
    builderIterators match {
      case bi if bi.nonEmpty && bi.last.isInstanceOf[Collector] =>
        bi.last.asInstanceOf[Collector].elements += elem
      case _ => addCollector(Iterator.single(elem))
    }
    this
  }
  override def ++=(xs: TraversableOnce[A]): this.type = {
    xs match {
      case lt: LongTraversableLike[_, _] => builderIterators += (() => lt.iterator)
      case xs if xs.isTraversableAgain => builderIterators += (() => xs.toIterator)
      case xs if builderIterators.nonEmpty && builderIterators.last.isInstanceOf[Collector] =>
        builderIterators.last.asInstanceOf[Collector].elements ++= xs
      case xs => {
        addCollector(xs)
      }
    }
    this
  }
  class Collector extends Function0[Iterator[A]] {
    val elements = Queue.empty[A]
    def apply() = elements.toIterator
  }
}
private class CompositeIteratorLongTraversable[A](builderIterators: Seq[() => Iterator[A]]) extends LongTraversable[A] {
  def iterator = {
    val iterators = Queue(builderIterators: _*)
    if (iterators.isEmpty) CloseableIterator.empty[A]
    else new CloseableIterator[A] {
      val toClose = Queue.empty[Iterator[A]]
      var now = iterators.dequeue.apply()
      def next = now.next
      def hasNext = {
        if (now.hasNext) true
        else if (iterators.isEmpty) false
        else {
          toClose += now
          now = iterators.dequeue().apply()
          hasNext
        }
      }
      def doClose = {
        import CloseableIterator.safeClose
        safeClose(now)
        toClose.foreach(safeClose)
        iterators.map(_.apply()).foreach(safeClose)
      }
    }
  }
}