/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection._
import generic.CanBuildFrom
import util.control.Breaks._
import CloseableIterator.managed
/**
 * The control signals for the limitFold method in [[scalax.io.LongTraversable]].
 *
 * These control when the fold terminates
 *
 * @param result the value to either return from method to to the next stage of the fold
 * @tparam A the type of Traversable this FoldResult can be used with
 */
sealed abstract class FoldResult[+A](val result:A)

/**
 * Signal indicating that the fold should continue to process another value
 */
case class Continue[+A](currentResult:A) extends FoldResult(currentResult)

/**
 * Signal indicating that the fold should stop and return the contained result
 */
case class End[+A](endResult:A) extends FoldResult(endResult)

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

  protected[io] def iterator:CloseableIterator[A]

  def foreach[U](f: (A) => U) {
    managed(iterator).acquireAndGet(_.foreach(f))
  }

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

  /** Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  A1    the type of the first half of the returned pairs (this is always a supertype
   *                 of the collection's element type `A`).
   *  @tparam  B     the type of the second half of the returned pairs
   *  @tparam  That  $zipthatinfo
   *  @param   bf    $zipbfinfo
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the minimum of the lengths of this $coll and `that`.
   *
   *  @usecase def zip[B](that: Iterable[B]): $Coll[(A, B)]
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new $coll containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the minimum of the lengths of this $coll and `that`.
   */
  def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That =
    doZip(that.iterator)

  /** Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  A1    the type of the first half of the returned pairs (this is always a supertype
   *                 of the collection's element type `A`).
   *  @tparam  B     the type of the second half of the returned pairs
   *  @tparam  That  $zipthatinfo
   *  @param   bf    $zipbfinfo
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the minimum of the lengths of this $coll and `that`.
   *
   *  @usecase def zip[B](that: Iterable[B]): $Coll[(A, B)]
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new $coll containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the minimum of the lengths of this $coll and `that`.
   */
  def zip[A1 >: A, B, That](that: LongTraversable[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That =
    managed(that.iterator).acquireAndGet(i => doZip(i))

  private def doZip[A1 >: A, B, That](those: Iterator[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val b = bf(repr)
    val these = this.iterator
    managed(this.iterator).acquireAndGet{these =>
      while (these.hasNext && those.hasNext)
        b += ((these.next, those.next))
      b.result
    }
  }


  /** Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is shorter than the other,
   *  placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   *  $orderDependent
   *
   *  @param that     the iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
   *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
   *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
   *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
   *
   *  @usecase def zipAll[B](that: Iterable[B], thisElem: A, thatElem: B): $Coll[(A, B)]
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
   *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new $coll containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
   *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
   *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
   */
  def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That =
    doZipAll(that.iterator,thisElem,thatElem)

  /** Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is shorter than the other,
   *  placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   *  $orderDependent
   *
   *  @param that     the iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
   *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
   *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
   *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
   *
   *  @usecase def zipAll[B](that: Iterable[B], thisElem: A, thatElem: B): $Coll[(A, B)]
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
   *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new $coll containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
   *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
   *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
   */
  def zipAll[B, A1 >: A, That](that: LongTraversable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That =
    managed(that.iterator).acquireAndGet(those => doZipAll(those,thisElem,thatElem))

  private def doZipAll[B, A1 >: A, That](those: Iterator[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val b = bf(repr)
    managed(this.iterator).acquireAndGet{ these =>
      while (these.hasNext && those.hasNext)
        b += ((these.next, those.next))
      while (these.hasNext)
        b += ((these.next, thatElem))
      while (those.hasNext)
        b += ((thisElem, those.next))
      b.result
    }
  }

  /** Zips this $coll with its indices.
   *
   *  $orderDependent
   *
   *  @tparam  A1    the type of the first half of the returned pairs (this is always a supertype
   *                 of the collection's element type `A`).
   *  @tparam  That  the class of the returned collection. Where possible, `That` is
   *    the same class as the current collection class `Repr`, but this
   *    depends on the element type `(A1, Int)` being admissible for that class,
   *    which means that an implicit instance of type `CanBuildFrom[Repr, (A1, Int), That]`.
   *    is found.
   *  @tparam  bf    an implicit value of class `CanBuildFrom` which determines the
   *    result class `That` from the current representation type `Repr`
   *    and the new element type `(A1, Int)`.
   *  @return        A new collection of type `That` containing pairs consisting of all elements of this
   *                 $coll paired with their index. Indices start at `0`.
   *
   *  @usecase def zipWithIndex: $Coll[(A, Int)]
   *
   *  @return        A new $coll containing pairs consisting of all elements of this
   *                 $coll paired with their index. Indices start at `0`.
   *  @example
   *    `List("a", "b", "c").zipWithIndex = List(("a", 0), ("b", 1), ("c", 2))`
   *
   */
  def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, Int), That]): That = {
    val b = bf(repr)
    var i = 0
    for (x <- this) {
      b += ((x, i))
      i +=1
    }
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
  def lslice(from: Long, until: Long): Repr = ldrop(from).ltake(0L max (until - (0L max from)))
  /**
   * The long equivalent of Traversable.splitAt
   */
  def lsplitAt(n: Long): (Repr, Repr) = (ltake(n), ldrop(n))
  /**
   * The long equivalent of Traversable.take
   */
  def ltake(n: Long) : Repr = {
    val b = newBuilder
    managed(iterator).acquireAndGet{iter =>
      var c = 0L
      while (c < n && iter.hasNext) {
        b += iter.next()
        c += 1
      }
    }
    b.result
  }

  override def view = new LongTraversableView[A,Repr] {
    protected lazy val underlying = self.repr

    protected[io] def iterator = self.iterator
  }
  override def view(from: Int, until: Int) = view.slice(from, until)
  /**
   * The long equivalent of Traversable.view(from,to)
   */
  def lview(from: Long, until: Long) : LongTraversableView[A,Repr] = this.view.lslice(from, until)


  def sameElements[B >: A](that:Iterable[B]):Boolean =
    managed(iterator).acquireAndGet(_.sameElements(that.iterator))

  def sameElements[B >: A](that:LongTraversable[B]):Boolean = {
    managed(iterator).acquireAndGet {these =>
      managed(that.iterator).acquireAndGet{ those =>
        these.sameElements(those)
      }
    }
  }


  /** Selects an element by its index in the $coll.
   *
   *  @param  idx  The index to select.
   *  @return the element of this $coll at index `idx`, where `0` indicates the first element.
   *  @throws `IndexOutOfBoundsException` if `idx` does not satisfy `0 <= idx < length`.
   */
  def apply (idx: Long):A = ldrop(idx).head

  /** Tests whether every element of this $coll relates to the
   *  corresponding element of another sequence by satisfying a test predicate.
   *
   *  @param   that  the other sequence
   *  @param   p     the test predicate, which relates elements from both sequences
   *  @tparam  B     the type of the elements of `that`
   *  @return  `true` if both sequences have the same length and
   *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
   *                  and `y` of `that`, otherwise `false`.
   */
  def corresponds[B](that: Seq[B])(p: (A,B) => Boolean): Boolean = {
    doCorresponds(that.iterator,p)
  }

  /** Tests whether every element of this $coll relates to the
   *  corresponding element of another sequence by satisfying a test predicate.
   *
   *  @param   that  the other sequence
   *  @param   p     the test predicate, which relates elements from both sequences
   *  @tparam  B     the type of the elements of `that`
   *  @return  `true` if both sequences have the same length and
   *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
   *                  and `y` of `that`, otherwise `false`.
   */
  def corresponds[B](that: LongTraversable[B])(p: (A,B) => Boolean): Boolean = {
    val iter = that.iterator
    try doCorresponds(iter,p)
    finally iter.close()
  }
  private def doCorresponds[B](j: Iterator[B],p: (A,B) => Boolean): Boolean = {
    val i = this.iterator
    try {
      while (i.hasNext && j.hasNext)
        if (!p(i.next(), j.next))
          return false

      !i.hasNext && !j.hasNext
    } finally {
      i.close()
    }

  }

  def indexWhere(p: A => Boolean): Long = indexWhere(p, 0)

  /** Finds index of the first element satisfying some predicate after or at some start index.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def indexWhere(p: A => Boolean, from: Long): Long = {
    var i = from
    var it = ldrop(from).iterator
    try {
      while (it.hasNext) {
        if (p(it.next())) return i
        else i += 1
      }
    }finally {
      it.close()
    }

    -1
  }

  def isDefinedAt ( idx : Long ) : Boolean = (idx >= 0) && (idx < lsize)
  /** Finds index of first occurrence of some value in this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index of the first element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def indexOf(elem: A): Int
   */
  def indexOf[B >: A](elem: B): Long = indexOf(elem, 0)

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
   *
   *  $mayNotTerminateInf
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def indexOf(elem: A, from: Int): Int
   */
  def indexOf[B >: A](elem: B, from: Long): Long = indexWhere(elem ==, from)

  /** Finds index of last occurrence of some value in this $coll.
   *
   *  $willNotTerminateInf
   *
   *  @param   elem   the element value to search for.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index of the last element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def lastIndexOf(elem: A): Int
   */
  def lastIndexOf[B >: A](elem: B): Long = lastIndexWhere(elem ==)

  /** Finds index of last occurrence of some value in this $coll before or at a given end index.
   *
   *  @param   elem   the element value to search for.
   *  @param   end    the end index.
   *  @tparam  B      the type of the element `elem`.
   *  @return  the index `<= end` of the last element of this $coll that is equal (wrt `==`)
   *           to `elem`, or `-1`, if none exists.
   *
   *  @usecase def lastIndexOf(elem: A, end: Int): Int
   */
  def lastIndexOf[B >: A](elem: B, end: Long): Long = lastIndexWhere(elem ==, end)

  /** Finds index of last element satisfying some predicate.
   *
   *  $willNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean): Long = lastIndexWhere(p, - 1)

  /** Finds index of last element satisfying some predicate before or at given end index.
   *
   * Always takes linear time and traverses entire traversal
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean, end: Long): Long = {
    var last = -1L
    var i = 0L;
    breakable{
      foreach{next =>
        if(end > 0 && i > end) break
        if(p(next)) last = i
        i += 1
      }
    }
    last
  }
  /** Computes length of longest segment whose elements all satisfy some predicate.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from  the index where the search starts.
   *  @return  the length of the longest segment of this $coll starting from index `from`
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def segmentLength ( p : (A) ⇒ Boolean , from : Long = 0 ) : Long =
    ldrop(from).limitFold(0){
      case (acc,next) if p(next) => Continue(acc + 1)
      case (acc,next) => End(acc)
    }

  /** Returns the length of the longest prefix whose elements all satisfy some predicate.
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the length of the longest prefix of this $coll
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def prefixLength(p: A => Boolean) = segmentLength(p, 0)

  private def doStartsWith[B](those:Iterator[B],offset:Long) = {
    breakable{
      ldrop(offset) foreach {next =>
        if(!those.hasNext || those.next != next) break
      }
    }
    !those.hasNext
  }

  /** Tests whether this $coll contains the given sequence at a given index.
   *
   * If the both the receiver object, <code>this</code> and
   * the argument, <code>that</code> are infinite sequences
   * this method may not terminate.
   *
   * @param  that    the sequence to test
   * @param  offset  the index where the sequence is searched.
   * @return `true` if the sequence `that` is contained in this $coll at index `offset`,
   *         otherwise `false`.
   */
  def startsWith[B](that: LongTraversable[B], offset: Long): Boolean =
    managed(that.iterator).acquireAndGet(i => doStartsWith(i,offset))

  /** Tests whether this $coll starts with the given sequence.
   *
   * @param  that    the sequence to test
   * @return `true` if this collection has `that` as a prefix, `false` otherwise.
   */
  def startsWith[B](that: LongTraversable[B]): Boolean = startsWith(that, 0)

  /** Tests whether this $coll contains the given sequence at a given index.
   *
   * If the both the receiver object, <code>this</code> and
   * the argument, <code>that</code> are infinite sequences
   * this method may not terminate.
   *
   * @param  that    the sequence to test
   * @param  offset  the index where the sequence is searched.
   * @return `true` if the sequence `that` is contained in this $coll at index `offset`,
   *         otherwise `false`.
   */
  def startsWith[B](that: Seq[B], offset: Long): Boolean = doStartsWith(that.iterator,offset)

  /** Tests whether this $coll starts with the given sequence.
   *
   * @param  that    the sequence to test
   * @return `true` if this collection has `that` as a prefix, `false` otherwise.
   */
  def startsWith[B](that: Seq[B]): Boolean = startsWith(that, 0)


  /** Finds first index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  the first index such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: Seq[B]): Long = indexOfSlice(that, 0)

  /** Finds first index after or at a start index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @param  from    the start index
   *  @return  the first index `>= from` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: Seq[B], from: Long): Long =
  if(that.isEmpty) from
  else {
    if (this.hasDefiniteSize && that.hasDefiniteSize)
      LongTraversableLike.indexOf(thisCollection, 0L, lsize, that, 0, that.length, from)
    else {
      var i = from
      var s: LongTraversable[A] = thisCollection ldrop i
      while (!s.isEmpty) {
        if (s startsWith that)
          return i

        i += 1
        s = s.tail
      }
      -1
    }
  }

  /** Tests whether this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  `true` if this $coll contains a slice with the same elements
   *           as `that`, otherwise `false`.
   */
  def containsSlice[B](that: Seq[B]): Boolean = indexOfSlice(that) != -1

  /** Tests whether this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  `true` if this $coll contains a slice with the same elements
   *           as `that`, otherwise `false`.
   */
  def containsSlice[B](that: Seq[B],start:Long): Boolean = indexOfSlice(that,start) != -1

  /** Groups elements in fixed size blocks by passing a "sliding window"
   *  over them (as opposed to partitioning them, as is done in grouped.)
   *
   *  This is based on Iterator#sliding but does not return an iterator
   *  to ensure that a resource is not left open.
   *
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive
   *         groups (defaults to 1)
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def sliding[B >: A](size: Int, step: Int=1): LongTraversable[Seq[A]] =
    LongTraversable[Seq[A]](
      self.iterator.modifiedSliding(size, step),
      "Sliding("+size+","+step+" LongTraversable(...)")


  /*
  Probably can be done but requires more work than I have time for

  def lastIndexOfSlice[B >: A](that: Seq[B]): Int = lastIndexOfSlice(that, length)
  def lastIndexOfSlice[B >: A](that: Seq[B], end: Int): Int =
  def endsWith[B](that: Seq[B]): Boolean = {
   */
  /*
  Don't think the following are possible without a lot of work
  def diff [B >: A] (that: Seq[B]) : Repr
  def distinct : Repr
  def intersect [B >: A] ( that : Seq[B] ) : Repr
   */

}

object LongTraversableLike {
    /**  A KMP implementation, based on the undoubtedly reliable wikipedia entry.
   *
   *  @author paulp
   *  @since  2.8
   */
  private def KMP[B](S: Seq[B], W: Seq[B]): Option[Int] = {
    // trivial cases
    if (W.isEmpty) return Some(0)
    else if (W drop 1 isEmpty) return (S indexOf W(0)) match {
      case -1 => None
      case x  => Some(x)
    }

    val T: Array[Int] = {
      val arr = new Array[Int](W.length)
      var pos = 2
      var cnd = 0
      arr(0) = -1
      arr(1) = 0
      while (pos < W.length) {
        if (W(pos - 1) == W(cnd)) {
          arr(pos) = cnd + 1
          pos += 1
          cnd += 1
        }
        else if (cnd > 0) {
          cnd = arr(cnd)
        }
        else {
          arr(pos) = 0
          pos += 1
        }
      }
      arr
    }

    var m, i = 0
    def mi = m + i

    while (mi < S.size) {
      if (W(i) == S(mi)) {
        i += 1
        if (i == W.size)
          return Some(m)
      }
      else {
        m = mi - T(i)
        if (i > 0)
          i = T(i)
      }
    }
    None
  }

  /** Finds a particular index at which one sequence occurs in another sequence.
   *  Both the source sequence and the target sequence are expressed in terms
   *  other sequences S' and T' with offset and length parameters.  This
   *  function is designed to wrap the KMP machinery in a sufficiently general
   *  way that all library sequence searches can use it.  It is unlikely you
   *  have cause to call it directly: prefer functions such as StringBuilder#indexOf
   *  and Seq#lastIndexOf.
   *
   *  @param  source        the sequence to search in
   *  @param  sourceOffset  the starting offset in source
   *  @param  sourceCount   the length beyond sourceOffset to search
   *  @param  target        the sequence being searched for
   *  @param  targetOffset  the starting offset in target
   *  @param  targetCount   the length beyond targetOffset which makes up the target string
   *  @param  fromIndex     the smallest index at which the target sequence may start
   *
   *  @return the applicable index in source where target exists, or -1 if not found
   */
  private def indexOf[B](
    source: LongTraversable[B], sourceOffset: Long, sourceCount: Long,
    target: Seq[B], targetOffset: Int, targetCount: Int,
    fromIndex: Long): Long = {
      val toDrop = fromIndex max 0
      val src = (source.lslice(sourceOffset, sourceCount) ldrop toDrop).toSeq
      val tgt = target.slice(targetOffset, targetCount)

      KMP(src, tgt) match {
        case None    => -1
        case Some(x) => x.toLong + toDrop
      }
  }

}
