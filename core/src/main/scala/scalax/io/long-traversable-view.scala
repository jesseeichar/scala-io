/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection._
import generic._
import TraversableView.NoBuilder

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

/**
 * The actual [[scalax.io.LongTraversableView]] implementations.  Not interesting API beyond what is exposed in
 * [[scalax.io.LongTraversableView]] and [[scalax.io.LongTraversable]]
 */
trait LongTraversableViewLike[+A, +Coll, +This <: LongTraversableView[A,Coll] with LongTraversableViewLike[A,Coll, This]]
      extends LongTraversable[A] with LongTraversableLike[A, This] with TraversableView[A,Coll] with TraversableViewLike[A,Coll,This]{
  self =>

  trait Transformed[+B] extends LongTraversableView[B, Coll] with super.Transformed[B]

  trait EmptyView extends Transformed[Nothing] with super.EmptyView

  trait Forced[B] extends super.Forced[B] with Transformed[B]

  trait LSliced extends Transformed[A] {
    protected[this] def from: Long
    protected[this] def until: Long
    override def foreach[U](f: A => U) {
      var index = 0
      for (x <- self) {
        if (from <= index) {
          if (until <= index) return
          f(x)
        }
        index += 1
      }
    }
  }

  trait Mapped[B] extends super.Mapped[B] with Transformed[B]
  trait FlatMapped[B] extends super.FlatMapped[B] with Transformed[B]
  trait Appended[B >: A] extends super.Appended[B] with Transformed[B]
  trait Filtered extends super.Filtered with Transformed[A]
  trait TakenWhile extends super.TakenWhile with Transformed[A]
  trait DroppedWhile extends super.DroppedWhile with Transformed[A]

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new { val forced = xs } with Forced[B]
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new { val rest = that } with Appended[B]
  protected override def newMapped[B](f: A => B): Transformed[B] = new { val mapping = f } with Mapped[B]
  protected override def newFlatMapped[B](f: A => TraversableOnce[B]): Transformed[B] = new { val mapping = f } with FlatMapped[B]
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new { val pred = p } with Filtered

// TODO Implement when slice interval can be accessed
//  protected def newSliced(_endpoints: SliceInterval): Transformed[A] = newLSliced(_endpoints.from.toLong, _endpoints.until.toLong)
  protected def newLSliced(_from: Long, _until: Long): Transformed[A] = new { override val from = _from; override val until = _until } with LSliced
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with  DroppedWhile
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with TakenWhile

  override def drop(n: Int): This = newLSliced(n max 0, Long.MaxValue).asInstanceOf[This]
  override def ldrop(n: Long): This = newLSliced(n max 0, Long.MaxValue).asInstanceOf[This]

  override def take(n: Int): This = newLSliced(0, n).asInstanceOf[This]
  override def ltake(n: Long): This = newLSliced(0, n).asInstanceOf[This]

  override def slice(from: Int, until: Int) = newLSliced(from,until).asInstanceOf[This]

  override def stringPrefix = "LongTraversableView"

}

