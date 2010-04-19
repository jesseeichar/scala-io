/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection._
import scala.collection.generic._


/**
 * A traversable for use on very large datasets which cannot be indexed with Ints but instead
 * require Longs for indexing.  This trait adds methods for accessing the extra portions
 * of the dataset.
 */
trait LongTraversableLike[+A, +Repr <: LongTraversableLike[A,Repr]] extends TraversableLike[A, Repr] {
  self =>


  override protected[this] def thisCollection: LongTraversable[A] = this.asInstanceOf[LongTraversable[A]]
  override protected[this] def toCollection(repr: Repr): LongTraversable[A] = repr.asInstanceOf[LongTraversable[A]]

  def lcount(p: A => Boolean): Long = {
      var cnt = 0L
      for (x : A <- this) if (p(x)) cnt += 1
      cnt
  }

  def ldrop(n: Long) : Repr = {
    def doDrop(remaining : Long, t : LongTraversableLike[A,Repr]) : LongTraversableLike[A,Repr] = {
      if(remaining < Int.MaxValue) t.drop(remaining.toInt)
      else doDrop(remaining - Int.MaxValue, t.drop(Int.MaxValue))
    }
    val b = newBuilder
    b ++= doDrop(n,this)
    b.result
  }

  override def hasDefiniteSize = false
  def lsize: Long = foldLeft(0L){(c,_) => c + 1}
  
  def lslice(from: Long, until: Long): Repr = ldrop(from).ltake(until)
  def lsplitAt(n: Long): (Repr, Repr) = (ltake(n), ldrop(n))
  
  def ltake(n: Long) : Repr = {
    val b = newBuilder
    val traversable = new Traversable[A] {
      def foreach[U](f: (A) => U): Unit = {
        import util.control.Breaks._
      
        breakable {
          var c = 0L
          for(i <- this) yield {
            if (c >= n) break;
            c += 1
            f(i)
          }
        }
      }
    }
    b ++= traversable
    b.result
  }

  override def view = new LongTraversableView[A,Repr] {
    protected lazy val underlying = self.repr
    def foreach[U](f: (A) => U): Unit = self foreach f
  }
  override def view(from: Int, until: Int) = view.slice(from, until)

//  def lview(from: Long, until: Long) : LongTraversableView[A,Repr] = this.view.lslice(from, until)
}
