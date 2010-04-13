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
trait LongTraversable[A] extends Traversable[A]
                            with GenericTraversableTemplate[A, LongTraversable] 
                            with TraversableLike[A, LongTraversable[A]] {

  override def companion : GenericCompanion[LongTraversable] = LongTraversable
  
  def longCount(p: A => Boolean): Long = {
      var cnt = 0L
      for (x : A <- this) if (p(x)) cnt += 1
      cnt
  }

  override def drop(n: Int) : LongTraversable[A] = super.drop (n)
  def drop(n: Long) : LongTraversable[A] = {
    def doDrop(remaining : Long, t : LongTraversable[A]) : LongTraversable[A] = {
      if(remaining < Int.MaxValue) t.drop(remaining.toInt)
      else doDrop(remaining - Int.MaxValue, t.drop(Int.MaxValue))
    }
    doDrop(n,this)
  }

  override def hasDefiniteSize = false
  def longSize: Long = foldLeft(0L){(c,_) => c + 1}
  
  def slice(from: Long, until: Long): LongTraversable[A] = drop(from).take(until)
  def splitAt(n: Long): (LongTraversable[A], LongTraversable[A]) = (take(n), drop(n))
  
  def take(n: Long) : LongTraversable[A] = new LongTraversable[A] {
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
}

object LongTraversable extends TraversableFactory[LongTraversable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LongTraversable[A]] = new GenericCanBuildFrom[A]
  
  // TODO consider a correct implementation
  def newBuilder[A] = new scala.collection.mutable.Builder[A,LongTraversable[A]] {
    val buf = new scala.collection.mutable.ListBuffer[A]()
    def +=(a:A) = {buf += a;this}
    def clear = buf.clear
    def result = new LongTraversable[A] {
      def foreach[U](f: A => U) = buf foreach f
    }
  }
}
