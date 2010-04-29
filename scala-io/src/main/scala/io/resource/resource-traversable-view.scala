/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io.{
  LongTraversableView, LongTraversableViewLike
}
import scala.collection._
import scala.collection.generic._
import TraversableView.NoBuilder


trait ResourceTraversableView[A, +Coll] extends ResourceTraversableViewLike[A, Coll, ResourceTraversableView[A, Coll]]

object ResourceTraversableView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, ResourceTraversableView[A, ResourceTraversable[_]]] = 
    new CanBuildFrom[Coll, A, ResourceTraversableView[A, ResourceTraversable[_]]] { 
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}

trait ResourceTraversableViewLike[A, +Coll, +This <: ResourceTraversableView[A,Coll] with ResourceTraversableViewLike[A,Coll, This]]
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
  trait Sliced extends Transformed[A] with super.Sliced {
    override def start = self.start + (from max 0)
    override def end = if( until > self.end ) self.end 
                       else safeSum(self.start,(until max 0))
    def conv = self.conv
    
    override def foreach[U](f: A => U) = doForeach(f)
  }
  trait Mapped[B] extends Transformed[B] with super.Mapped[B] {
    def conv = self.conv andThen mapping
  }
  trait TakenWhile extends Identity with super.TakenWhile
  trait DroppedWhile extends Identity with super.DroppedWhile


  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
//  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] {val mapping = f}
  protected override def newLSliced(_from: Long, _until: Long): Transformed[A] = new Sliced { def from = _from; def until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }


  override def ldrop(length : Long) = lslice(length, Long.MaxValue)
  override def ltake(length : Long) = lslice(0, safeSum(start,length))
  override def lslice(_start : Long, _end : Long) = newLSliced(safeSum(start,0L max _start), _end min end).asInstanceOf[This]

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

