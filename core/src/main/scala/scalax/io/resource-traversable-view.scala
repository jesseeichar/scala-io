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

/**
 * The actual implementation of the View optimized for working on Resources
 */
private[io] trait ResourceTraversableViewLike[A, +Coll, +This <: ResourceTraversableView[A,Coll] with ResourceTraversableViewLike[A,Coll, This]]
      extends ResourceTraversable[A] with LongTraversableView[A,Coll] with LongTraversableViewLike[A,Coll,This]{
  self =>


  trait Transformed[B] extends ResourceTraversableView[B, Coll] with super.Transformed[B] {
    type In = self.In
    type SourceOut = self.SourceOut
    def source = self.source
    def start = self.start
    def end = self.end
  }

  trait Identity extends Transformed[A] with super.Transformed[A] {
    def conv = self.conv
  }

//  trait Forced[B] extends Transformed[B] with super.Forced[B]
   trait LSliced extends super.LSliced with Transformed[A] {
    override def start = self.start + (from max 0)
    override def end = if( until > self.end ) self.end
                       else safeSum(self.start,(until max 0))
    def conv = self.conv

    override def iterator = getIterator
  }
  trait Mapped[B] extends Transformed[B] {
    val mapping: A => B
    def conv = self.conv andThen mapping
  }
  trait TakenWhile extends super.TakenWhile with Identity {
    override protected[io] def iterator:CloseableIterator[A] = getIterator.takeWhile(pred)
  }
  trait DroppedWhile extends super.DroppedWhile with Identity {
    override protected[io] def iterator:CloseableIterator[A] = getIterator.dropWhile(pred)
  }


  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
//  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected override def newMapped[B](f: A => B): Transformed[B] = new {val mapping = f} with Mapped[B]
  protected override def newLSliced(_from: Long, _until: Long): Transformed[A] = new { val from = _from; val until = _until } with LSliced
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with DroppedWhile
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new  { val pred = p } with TakenWhile


  override def slice(from: Int, until: Int) = lslice(from,until)

  override def lslice(_start : Long, _end : Long) = newLSliced(0L max _start, _end).asInstanceOf[This]

  override def stringPrefix = "ResourceTraversableView"
/*

  TODO Optimize these methods so that drop/slice after applying these methods will create a new ResourceTraversable
       with updated start values so that (for example) append followed by drop will drop the elements from the stream
       therefore reducing the number of elements read

  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B] {
    def conv = self.conv andThen up
    private def up(i:Traversable[A]):Traversable[B] = i flatMap mapping
  }
  trait Appended[B >: A] extends Transformed[B] with super.Appended[B] {
    def conv = self.conv
  }
  trait Filtered extends Identity with super.Filtered
*/
}

