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

trait LongTraversableView[+A, +Coll] extends LongTraversableViewLike[A, Coll, LongTraversableView[A, Coll]]

object LongTraversableView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LongTraversableView[A, LongTraversable[_]]] = 
    new CanBuildFrom[Coll, A, LongTraversableView[A, LongTraversable[_]]] { 
      def apply(from: Coll) = new NoBuilder 
      def apply() = new NoBuilder 
    }
}

trait LongTraversableViewLike[+A, +Coll, +This <: LongTraversableView[A,Coll] with LongTraversableViewLike[A,Coll, This]]
      extends LongTraversable[A] with LongTraversableLike[A, This] with TraversableView[A,Coll] with TraversableViewLike[A,Coll,This]{
  self =>

  trait Transformed[+B] extends LongTraversableView[B, Coll] with super.Transformed[B]

  trait Forced[B] extends Transformed[B] with super.Forced[B]

  trait Sliced extends Transformed[A] {
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

  trait Mapped[B] extends Transformed[B] with super.Mapped[B]

  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B]
  trait Appended[B >: A] extends Transformed[B] with super.Appended[B]
  trait Filtered extends Transformed[A] with super.Filtered
  trait TakenWhile extends Transformed[A] with super.TakenWhile
  trait DroppedWhile extends Transformed[A] with super.DroppedWhile 

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = newLSliced(_from.toLong, _until.toLong)
  protected def newLSliced(_from: Long, _until: Long): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
  
  override def drop(n: Int): This = newLSliced(n max 0, Long.MaxValue).asInstanceOf[This]
  override def ldrop(n: Long): This = newLSliced(n max 0, Long.MaxValue).asInstanceOf[This]
  
  override def take(n: Int): This = newLSliced(0, n).asInstanceOf[This]
  override def ltake(n: Long): This = newLSliced(0, n).asInstanceOf[This]
  
  override def stringPrefix = "LongTraversableView"
}
