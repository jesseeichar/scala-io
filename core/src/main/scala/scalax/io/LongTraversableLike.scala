/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection._

/**
 * The control signals for the limitFold method in [[scalax.io.LongTraversable]].
 *
 * These control when the fold terminates
 *
 * @param result the value to either return from method to to the next stage of the fold
 * @tparam A the type of Traversable this FoldResult can be used with
 */
sealed abstract class FoldResult[+A](result:A)

/**
 * Signal indicating that the fold should continue to process another value
 */
case class Continue[+A](result:A) extends FoldResult(result)

/**
 * Signal indicating that the fold should stop and return the contained result
 */
case class End[+A](result:A) extends FoldResult(result)


/**
 * A traversable for use on very large datasets which cannot be indexed with Ints but instead
 * require Longs for indexing.
 *
 * This trait adds methods for accessing the extra portions of the dataset.
 */
trait LongTraversableLike[+A, +Repr <: LongTraversableLike[A,Repr]] extends TraversableLike[A, Repr] {
  self =>

  override protected[this] def thisCollection: LongTraversable[A] = this.asInstanceOf[LongTraversable[A]]
  override protected[this] def toCollection(repr: Repr): LongTraversable[A] = repr.asInstanceOf[LongTraversable[A]]
  override def toArray[B >: A : ClassManifest] = toBuffer.toArray

  /**
   * A foldLeft operation that can be terminated without processing the entire collection.
   *
   * Unlike a normal fold, the function passed to limitFold returns a [[scalax.io.FoldResult]] which both provides the
   * value that is to be passed to the next stage of the fold as well as represents if the fold should continue
   * or terminate.
   *
   * @param init the value to seed the operation with.  IE the value that is passed as the accumulator for the first
   *             value of the fold operation
   * @param op the operation that combines the current and previous versions.  The input is the (acc,next) where
   *           acc is the result from the previous call and next is the next value in the collection to be processed.
   *           The return value of the op is Either [[scalax.io.Continue]] or [[scalax.io.End]] indicating if the
   *           process should continue to next element or terminate, returning the value contained in the result object
   * @return the last value contained in the [[scalax.io.FoldResult]] which was returned by op
   */
  def limitFold[U](init:U)(op:(U,A) => FoldResult[U]):U = {
    case class FoldTerminator(v:U) extends RuntimeException

    try {
      foldLeft(init){ (acc,next) =>
        op(acc,next) match {
          case Continue(result) => result
          case End(result) => throw new FoldTerminator(result)
        }
      }
    } catch {
      case FoldTerminator(v) => v
    }
  }

  /**
   * The long equivalent of count in Traversable.
   */
  override def slice(from: Int, until: Int) = lslice(from,until)

  def lcount(p: A => Boolean): Long = {
      var cnt = 0L
      for (x : A <- this) if (p(x)) cnt += 1
      cnt
  }

  /**
   * The long equivalent of Traversable.drop
   */
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
  /**
   * The long equivalent of Traversable.size
   *
   * NOT recommended for use since it might trigger a full traversal of the traversable
   */
  def lsize: Long = foldLeft(0L){(c,_) => c + 1}
  /**
   * The long equivalent of Traversable.slice
   */
  def lslice(from: Long, until: Long): Repr = ldrop(from).ltake(0L max until-from)
  /**
   * The long equivalent of Traversable.splitAt
   */
  def lsplitAt(n: Long): (Repr, Repr) = (ltake(n), ldrop(n))
  /**
   * The long equivalent of Traversable.take
   */
  def ltake(n: Long) : Repr = {
    val b = newBuilder
    import util.control.Breaks._

    breakable {
      var c = 0L
      foreach { i =>
        if (c >= n) break;
        c += 1
        b += i
      }
    }
    b.result
  }

  override def view = new LongTraversableView[A,Repr] {
    protected lazy val underlying = self.repr
    def foreach[U](f: (A) => U): Unit = self foreach f
  }
  override def view(from: Int, until: Int) = view.slice(from, until)
  /**
   * The long equivalent of Traversable.view(from,to)
   */
  def lview(from: Long, until: Long) : LongTraversableView[A,Repr] = this.view.lslice(from, until)
}
