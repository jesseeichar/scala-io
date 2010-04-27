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


trait StreamLongTraversableView[A, +Coll] extends StreamLongTraversableViewLike[A, Coll, StreamLongTraversableView[A, Coll]]

object StreamLongTraversableView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, StreamLongTraversableView[A, StreamLongTraversable[_]]] = 
    new CanBuildFrom[Coll, A, StreamLongTraversableView[A, StreamLongTraversable[_]]] { 
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
}

trait StreamLongTraversableViewLike[A, +Coll, +This <: StreamLongTraversableView[A,Coll] with StreamLongTraversableViewLike[A,Coll, This]]
      extends StreamLongTraversable[A] with LongTraversableView[A,Coll] with LongTraversableViewLike[A,Coll,This]{
  self =>

  trait Transformed[B] extends StreamLongTraversableView[B, Coll] with super.Transformed[B] {
    type In = self.In
    type SourceOut = self.SourceOut
    def source = self.source
    def start = self.start
    def end = self.end
  }
  
  trait Identity[A] extends StreamLongTraversableView[A, Coll] with super.Transformed[A] {
  }
    
//  trait Forced[B] extends Transformed[B] with super.Forced[B]
  trait Sliced extends Transformed[A] with super.Sliced {
    override def start = self.start + from
    override def end = until
    def conv = self.conv
    
    override def foreach[U](f: A => U) = doForeach(f)
  }
  trait Mapped[B] extends Transformed[B] with super.Mapped[B] {
    def conv = self.conv andThen up
    private def up(i:Traversable[A]):Traversable[B] = i map mapping
  }
  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B] {
    def conv = self.conv andThen up
    private def up(i:Traversable[A]):Traversable[B] = i flatMap mapping
  }
  trait Appended[B >: A] extends Transformed[B] with super.Appended[B] {
    def conv = self.conv
  }
  trait Filtered extends Transformed[A] with super.Filtered {
    def conv = self.conv
  }
  trait TakenWhile extends Transformed[A] with super.TakenWhile{
    def conv = self.conv
  }
  trait DroppedWhile extends Transformed[A] with super.DroppedWhile{
    def conv = self.conv
  }


  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
//  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] {val mapping = f}
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newLSliced(_from: Long, _until: Long): Transformed[A] = new Sliced { def from = _from; def until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }

  override def stringPrefix = "StreamLongTraversableView"
}

