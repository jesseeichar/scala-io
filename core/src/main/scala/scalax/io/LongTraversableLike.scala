/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import scala.collection.generic.CanBuildFrom
import scala.collection.Iterable
import scala.collection.Iterator
import scala.collection.Seq
import scala.collection.TraversableLike
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable
import scala.collection.mutable.Builder
import scala.collection.GenTraversableOnce
/**
 * The control signals for the limitFold method in [[scalax.io.LongTraversable]].
 *
 * These control when the fold terminates
 *
 * @param result the value to either return from method to to the next stage of the fold
 * @tparam A the type of Traversable this FoldResult can be used with
 */
sealed trait FoldResult[@specialized(Byte,Char) +A]{def result: A}

/**
 * Signal indicating that the fold should continue to process another value
 * 
 * @param skip the number of bytes to skip before processing a byte.  
 * IE if skip == 3 then the next 3 bytes will be skipped 
 */
case class Continue[@specialized(Byte,Char) +A](result: A,skip:Long = 0L) extends FoldResult[A]

/**
 * Signal indicating that the fold should stop and return the contained result
 */
case class End[@specialized(Byte,Char) +A](result: A) extends FoldResult[A]

/**
 * A traversable for use on very large datasets which cannot be indexed with Ints but instead
 * require Longs for indexing.
 *
 * This trait adds methods for accessing the extra portions of the dataset.
 */
trait LongTraversableLike[@specialized(Byte,Char) +A, +Repr <: LongTraversableLike[A, Repr]] extends TraversableLike[A, Repr] {
  self =>
  def context:ResourceContext
  override protected[this] def thisCollection: LongTraversable[A] = this.asInstanceOf[LongTraversable[A]]
  override protected[this] def toCollection(repr: Repr): LongTraversable[A] = repr.asInstanceOf[LongTraversable[A]]
  override def toArray[B >: A: ClassManifest] = 
    if(hasDefiniteSize && size <= Int.MaxValue) { 
      val array = new Array[B](size.toInt)
      var i = 0
      foreach { a => 
        array(i) = a
        i += 1
      }
      array
    } else {
      toBuffer.toArray
  }
  def processor = new processing.CloseableIteratorProcessor(() => iterator, context)

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
  def limitFold[U](init: U)(op: (U, A) => FoldResult[U]): U = {
    class FoldTerminator(val value: U) extends RuntimeException
    try {
      val result = foldLeft((init,0L)) { (acc, next) =>
        if(acc._2 > 0 ) (acc._1, acc._2 - 1)
        else {
	        op(acc._1, next) match {
	          case Continue(result,skip) => (result,skip)
	          case End(result) => throw new FoldTerminator(result)
	        }
        }
      }
      result._1
    } catch {
      case ft:FoldTerminator => ft.value
    }
  }
  
  /**
   * Use the underlying iterator for this traversable.  
   * 
   * Note:  If the iterator is returned from this block an exception will be thrown 
   * because the iterator is invalid outside of this block and the behaviour is undefined
   * 
   * @throws AssertionError if the iterator is returned
   */
  def withIterator[U](f: CloseableIterator[A] => U) = {
    val iter = iterator
    /*var closeExceptions: List[Throwable] = Nil
    val catcher = util.control.Exception.allCatch.andFinally(closeExceptions = iter.close())
    
    val result = catcher.either {
      val result = f(iter)
      assert(System.identityHashCode(result) != System.identityHashCode(iter), 
          "the iterator may not escape the bounds of this block")
      result
    }

    Right {
      if (result.left.toOption ++ closeExceptions nonEmpty) {
        context.errorHandler(result, closeExceptions)
      } else {
        result.right.get
      }
    }*/
    try {
      val result = f(iter)
      assert(System.identityHashCode(result) != System.identityHashCode(iter), 
          "the iterator may not escape the bounds of this block")
      result
    }
    finally iter.close()
  }
  
  protected[io] def iterator: CloseableIterator[A]

  def foreach[@specialized(Unit) U](f: (A) => U) = withIterator { _.foreach(f) }

  override def head = headOption.getOrElse (throw new java.util.NoSuchElementException("head of an empty traversable"))
  override def headOption = withIterator { iterator =>
    if (iterator.hasNext) Some(iterator.next()) else None
  }

  def lcount(p: A => Boolean): Long = withIterator { iter =>
    var cnt = 0L
    while(iter.hasNext) if (p(iter.next)) cnt += 1
    cnt
  }

  private def build[B >: A](f: CloseableIteratorOps[A] => CloseableIterator[B]) : Repr = newBuilder match {
    case ltf:LongTraversableBuilder[A,Repr] => ltf.fromIterator(f(CloseableIteratorOps(iterator)).asInstanceOf[CloseableIterator[A]], context)
    case b => 
      withIterator(iter => b ++= f(CloseableIteratorOps(iter)).asInstanceOf[CloseableIterator[A]])
      b.result()
  }

  /**
   * The long equivalent of Traversable.drop
   */
  def ldrop(n: Long): Repr = lslice(n,Long.MaxValue)
  override /*TraversableLike*/ def drop(n: Int): Repr = ldrop(n)
  override /*TraversableLike*/ def dropWhile(p: A => Boolean): Repr = build(_.dropWhile(p))
  override /*TraversableLike*/ def takeWhile(p: A => Boolean): Repr = build(_.takeWhile(p))
  
  /**
   * The long equivalent of Traversable.take
   */
  def ltake(n: Long): Repr =  lslice(0, n)
  override /*TraversableLike*/ def take(n: Int): Repr = ltake(n)
  
  def lslice(from: Long, until:Long): Repr = build(_.lslice(from,until))
  override /*TraversableLike*/ def slice(from:Int, until:Int) = lslice(from,until)
  

  override /*TraversableLike*/ def forall(p: A => Boolean): Boolean = 
    withIterator(_ forall p)
  override /*TraversableLike*/ def exists(p: A => Boolean): Boolean = 
    withIterator(_ exists p)
  override /*TraversableLike*/ def find(p: A => Boolean): Option[A] = 
    withIterator(_ find p)
  override /*TraversableLike*/ def isEmpty: Boolean = 
    !withIterator(_.hasNext)
    
  override /*TraversableLike*/ def foldRight[B](z: B)(op: (A, B) => B): B =
    withIterator(_.foldRight(z)(op))
  override /*TraversableLike*/ def reduceRight[B >: A](op: (A, B) => B): B = 
    withIterator(_.reduceRight(op))
    
  override /*TraversableLike*/ def filter(p: A => Boolean): Repr = build(_ filter p)
  override /*TraversableLike*/ def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = bf(repr) match {
    case ltf:LongTraversableBuilder[B,That] => ltf.fromIterator(CloseableIteratorOps(iterator).map(f), context)
    case b => 
      withIterator(i => b++= i.map(f))
      b.result()
  }
    override /*TraversableLike*/ def ++:[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = 
    bf(repr) match {
	    case ltf:LongTraversableBuilder[B,That] => ltf.fromIterator(CloseableIteratorOps(iterator) ++ that, context)
	    case b => 
	      withIterator(it => b ++= it)
	      b ++= that
	      b.result()      
	    }
override /*TraversableLike*/ def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = 
  bf(repr) match {
    case ltf:LongTraversableBuilder[B,That] => ltf.fromIterator(CloseableIteratorOps(iterator).flatMap(f), context)
    case b => 
      withIterator(i => b ++= i.flatMap(f))
      b.result()
  }
  override /*TraversableLike*/ def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That =  
  bf(repr) match {
    case ltf:LongTraversableBuilder[B,That] => ltf.fromIterator(CloseableIteratorOps(iterator).collect(pf),context)
    case b => 
      
withIterator(i => b ++= i.collect(pf))
      b.result()
  } 

    /**
   * Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the minimum of the lengths of this $coll and `that`.
   */
  def zip[B,A1 >: A, That](that: Iterable[B])(implicit bf:LongTraversableBuilder[(A1,B),That]): That =
    doZip(that.iterator)(bf)

  /**
   * Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is longer than the other, its remaining elements are ignored.
   *
   *  @param   that  The iterable providing the second half of each result pair
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the minimum of the lengths of this $coll and `that`.
   */
  def zip[B,A1 >: A, That](that: LongTraversable[B])(implicit bf:LongTraversableBuilder[(A1,B),That]): That =
    doZip(that.iterator)(bf)

  private def doZip[B,A1 >: A, That](those: => Iterator[B])(bf:LongTraversableBuilder[(A1,B),That]): That = 
    bf.fromIterator(CloseableIteratorOps(self.iterator).zip(those), context)

  /**
   * Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is shorter than the other,
   *  placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   *  $orderDependent
   *
   *  @param that     the iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
   *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
   *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
   *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf:LongTraversableBuilder[(A1,B),That]): That =
    doZipAll(that.iterator, thisElem, thatElem)(bf)

  /**
   * Returns a $coll formed from this $coll and another iterable collection
   *  by combining corresponding elements in pairs.
   *  If one of the two collections is shorter than the other,
   *  placeholder elements are used to extend the shorter collection to the length of the longer.
   *
   *  $orderDependent
   *
   *  @param that     the iterable providing the second half of each result pair
   *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
   *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
   *  @tparam  B     the type of the second half of the returned pairs
   *  @return        a new collection of type `That` containing pairs consisting of
   *                 corresponding elements of this $coll and `that`. The length
   *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
   *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
   *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
   *
   */
  def zipAll[B, A1 >: A, That](that: LongTraversable[B], thisElem: A1, thatElem: B)(implicit bf:LongTraversableBuilder[(A1,B),That]): That =
    doZipAll(that.iterator, thisElem, thatElem)(bf)

  private def doZipAll[B, A1 >: A, That](those: => Iterator[B], thisElem: A1, thatElem: B)(bf:LongTraversableBuilder[(A1,B),That]): That = 
  	bf.fromIterator(CloseableIteratorOps(self.iterator).zipAll(those, thisElem, thatElem), context)

  /**
   * Zips this $coll with its indices.
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
  def zipWithIndex[A1 >: A, That](implicit bf: LongTraversableBuilder[(A1, Int), That]): That =
    bf.fromIterator(CloseableIteratorOps(iterator).zipWithIndex, context)

  override def hasDefiniteSize = false
  /**
   * The long equivalent of Traversable.size
   *
   * NOT recommended for use since it might trigger a full traversal of the traversable
   */
  def lsize: Long = withIterator{iter =>
    var c = 0L
    while(iter.hasNext) {
     iter.next
     c += 1
    }
    c
  }
  override def size = lsize min Integer.MAX_VALUE toInt
  /**
   * The long equivalent of Traversable.splitAt
   */
  def lsplitAt(n: Long): (Repr, Repr) = (ltake(n), ldrop(n))
    

  def sameElements[B >: A](that: Iterable[B]): Boolean =
    withIterator(_.sameElements(that.iterator))

  def sameElements[B >: A](that: LongTraversable[B]): Boolean = withIterator { these =>
    that.withIterator { those =>
      these.sameElements(those)
    }
  }

  /**
   * Selects an element by its index in the $coll.
   *
   *  @param  idx  The index to select.
   *  @return the element of this $coll at index `idx`, where `0` indicates the first element.
   *  @throws `IndexOutOfBoundsException` if `idx` does not satisfy `0 <= idx < length`.
   */
  def apply(idx: Long): A = 
    ldrop(idx).head

  /**
   * Tests whether every element of this $coll relates to the
   *  corresponding element of another sequence by satisfying a test predicate.
   *
   *  @param   that  the other sequence
   *  @param   p     the test predicate, which relates elements from both sequences
   *  @tparam  B     the type of the elements of `that`
   *  @return  `true` if both sequences have the same length and
   *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
   *                  and `y` of `that`, otherwise `false`.
   */
  def corresponds[B](that: Seq[B])(p: (A, B) => Boolean): Boolean = {
    doCorresponds(that.iterator, p)
  }

  /**
   * Tests whether every element of this $coll relates to the
   *  corresponding element of another sequence by satisfying a test predicate.
   *
   *  @param   that  the other sequence
   *  @param   p     the test predicate, which relates elements from both sequences
   *  @tparam  B     the type of the elements of `that`
   *  @return  `true` if both sequences have the same length and
   *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
   *                  and `y` of `that`, otherwise `false`.
   */
  def corresponds[B](that: LongTraversable[B])(p: (A, B) => Boolean): Boolean = that.withIterator {iter =>
    try doCorresponds(iter, p)
    finally iter.close()
  }
  private def doCorresponds[B](j: Iterator[B], p: (A, B) => Boolean): Boolean = withIterator { i =>
      while (i.hasNext && j.hasNext)
        if (!p(i.next(), j.next))
          return false
      !i.hasNext && !j.hasNext
  }

  def indexWhere(p: A => Boolean): Long = indexWhere(p, 0)

  /**
   * Finds index of the first element satisfying some predicate after or at some start index.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from   the start index
   *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def indexWhere(p: A => Boolean, from: Long): Long = 
    ldrop(from).withIterator { it =>
      var i = from
      while (it.hasNext) {
        if (p(it.next())) return i
        else i += 1
      }
      -1
    }

  def isDefinedAt(idx: Long): Boolean = (idx >= 0) && (idx < lsize)
  /**
   * Finds index of first occurrence of some value in this $coll.
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

  /**
   * Finds index of first occurrence of some value in this $coll after or at some start index.
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

  /**
   * Finds index of last occurrence of some value in this $coll.
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

  /**
   * Finds index of last occurrence of some value in this $coll before or at a given end index.
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

  /**
   * Finds index of last element satisfying some predicate.
   *
   *  $willNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean): Long = lastIndexWhere(p, -1)

  /**
   * Finds index of last element satisfying some predicate before or at given end index.
   *
   * Always takes linear time and traverses entire traversal
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
   *           or `-1`, if none exists.
   */
  def lastIndexWhere(p: A => Boolean, end: Long): Long = {
    
    (if(end<0) this else ltake(end+1)).withIterator { iter =>
      var last = -1L
      var i = 0L;
      while (iter.hasNext) {
        if (p(iter.next)) last = i
        i += 1
      }
      last
    }
  }
  /**
   * Computes length of longest segment whose elements all satisfy some predicate.
   *
   *  @param   p     the predicate used to test elements.
   *  @param   from  the index where the search starts.
   *  @return  the length of the longest segment of this $coll starting from index `from`
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def segmentLength(p: (A) => Boolean, from: Long = 0): Long =
    ldrop(from).limitFold(0) {
      case (acc, next) if p(next) => Continue(acc + 1)
      case (acc, next) => End(acc)
    }

  /**
   * Returns the length of the longest prefix whose elements all satisfy some predicate.
   *
   *  @param   p     the predicate used to test elements.
   *  @return  the length of the longest prefix of this $coll
   *           such that every element of the segment satisfies the predicate `p`.
   */
  def prefixLength(p: A => Boolean) = segmentLength(p, 0)

  private def doStartsWith[B](those: Iterator[B], offset: Long) = {
    breakable {
      ldrop(offset) foreach { next =>
        if (!those.hasNext || those.next != next) break
      }
    }
    !those.hasNext
  }

  /**
   * Tests whether this $coll contains the given sequence at a given index.
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
    that.withIterator(i => doStartsWith(i, offset))

  /**
   * Tests whether this $coll starts with the given sequence.
   *
   * @param  that    the sequence to test
   * @return `true` if this collection has `that` as a prefix, `false` otherwise.
   */
  def startsWith[B](that: LongTraversable[B]): Boolean = startsWith(that, 0)

  /**
   * Tests whether this $coll contains the given sequence at a given index.
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
  def startsWith[B](that: Seq[B], offset: Long): Boolean = doStartsWith(that.iterator, offset)

  /**
   * Tests whether this $coll starts with the given sequence.
   *
   * @param  that    the sequence to test
   * @return `true` if this collection has `that` as a prefix, `false` otherwise.
   */
  def startsWith[B](that: Seq[B]): Boolean = startsWith(that, 0)

  /**
   * Finds first index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  the first index such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: Seq[B]): Long = indexOfSlice(that, 0)

  /**
   * Finds first index after or at a start index where this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @param  from    the start index
   *  @return  the first index `>= from` such that the elements of this $coll starting at this index
   *           match the elements of sequence `that`, or `-1` of no such subsequence exists.
   */
  def indexOfSlice[B >: A](that: Seq[B], from: Long): Long =
    if (that.isEmpty) from
    else {
      if (this.hasDefiniteSize && that.hasDefiniteSize)
        LongTraversableLike.indexOf(thisCollection, 0L, lsize, that, 0, that.length, from)
      else {
        var i = from

        var s: LongTraversable[A] = thisCollection ldrop i
        while (s.nonEmpty) {
          if (s startsWith that)
            return i

          i += 1
          s = s.tail
        }
        -1
      }
    }

  /**
   * Tests whether this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  `true` if this $coll contains a slice with the same elements
   *           as `that`, otherwise `false`.
   */
  def containsSlice[B](that: Seq[B]): Boolean = indexOfSlice(that) != -1

  /**
   * Tests whether this $coll contains a given sequence as a slice.
   *  $mayNotTerminateInf
   *  @param  that    the sequence to test
   *  @return  `true` if this $coll contains a slice with the same elements
   *           as `that`, otherwise `false`.
   */
  def containsSlice[B](that: Seq[B], start: Long): Boolean = indexOfSlice(that, start) != -1

  /**
   * Groups elements in fixed size blocks by passing a "sliding window"
   *  over them.
   *
   *  This is based on Iterator#sliding but does not return an iterator
   *  to ensure that a resource is not left open.
   *
   *  @param size the number of elements per group
   *  @param step the distance between the first elements of successive
   *         groups (defaults to 1)
   *  @return An LongTraversable producing Seqs of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def sliding[That](size: Int, step: Int = 1)(implicit bf: CanBuildFrom[Repr, Seq[A], That]): That = bf(repr) match {
    case ltf:LongTraversableBuilder[Seq[A],That] => ltf.fromIterator(CloseableIteratorOps(iterator).modifiedSliding(size,step), context)
    case b => 
      b ++= withIterator(iter => CloseableIteratorOps(iter).modifiedSliding(size,step))
      b.result()
  }
  /**
   * Partitions the data into fixed size blocks (same as sliding(size,size).
   *
   *  @param size the number of elements per group
   *  @return An LongTraversable producing Seqs of size `size`, except the
   *          last and the only element will be truncated if there are
   *          fewer elements than size.
   */
  def grouped[That](size: Int)(implicit bf: CanBuildFrom[Repr, Seq[A], That]) = sliding(size, size)
  override /*TraversableLike*/ def partition(p: A => Boolean)= (filter(p), filterNot(p))
  override /*TraversableLike*/ def splitAt(n:Int): (Repr, Repr) = lsplitAt(n)
  override /*TraversableLike*/ def span(p: A => Boolean) = (takeWhile(p), dropWhile(p))

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
  
  override def init = build(_.init)
  
  def force:Repr

  /**
   * LongTraversable is a view so this method is not necessary
   */
//  override def view = super.view
  /**
   * LongTraversable is a view so this method is not necessary
   */
//  override def view(from: Int, until: Int) = view.slice(from, until)

}

object LongTraversableBuilder {
	implicit def longTraversableBuilder[A]:LongTraversableBuilder[A,LongTraversable[A]] = 
			LongTraversable.newBuilder[A]

}
trait LongTraversableBuilder[-A,+Repr] extends Builder[A, Repr] {
  def fromIterator(iterator: =>CloseableIterator[A], resourceContext: ResourceContext):Repr
  def result(resourceContext: ResourceContext):Repr
}

object LongTraversableLike {
  /**
   * A KMP implementation, based on the undoubtedly reliable wikipedia entry.
   *
   *  @author paulp
   *  @since  2.8
   */
  private def KMP[B](S: Seq[B], W: Seq[B]): Option[Int] = {
    // trivial cases
    if (W.isEmpty) return Some(0)
    else if (W drop 1 isEmpty) return (S indexOf W(0)) match {
      case -1 => None
      case x => Some(x)
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
        } else if (cnd > 0) {
          cnd = arr(cnd)
        } else {
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
      } else {
        m = mi - T(i)
        if (i > 0)
          i = T(i)
      }
    }
    None
  }

  /**
   * Finds a particular index at which one sequence occurs in another sequence.
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
      case None => -1
      case Some(x) => x.toLong + toDrop
    }
  }

}
