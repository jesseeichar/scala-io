package scalax.io

import akka.dispatch.Future

/**
 * An additional API for LongTraversable for asynchronous interaction
 * with the LongTraversable object.
 *
 * This class is not a Traversable even though the name implies that, it
 * is instead an API that provides asynchronous access to the
 * LongTraversable.
 *
 * Most of the blocking methods in LongTraversable (like foreach, find, fold)
 * are in this class and returns Futures instead of blocking and returning
 * the actual value like in LongTraversable.
 *
 * While non-blocking methods like map, filter are not present in the Async API because
 * those can be safely done with LongTraversable.
 *
 * The following example illustrates a non-blocking
 *
 * All methods in this class are performed asynchronously from the
 * calling thread.
 *
 * User: jeichar
 * Date: 3/18/12
 * Time: 8:26 PM
 */
class AsyncLongTraversable[+A](traversable:LongTraversable[A]) {
  private[this] implicit val executionContext = scalax.io.executionContext
  def /:[B](z: B)(op: (B, A) => B) = Future{traversable./:(z)(op)}
  def :\[B](z: B)(op: (A, B) => B) = Future{traversable.:\(z)(op)}
  def addString(b: StringBuilder, start: String, sep: String, end: String) = Future{traversable.addString(b,start,sep,end)}
  def addString(b: StringBuilder, sep: String) = Future{traversable.addString(b,sep)}
  def addString(b: StringBuilder) = Future{traversable.addString(b)}
  def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B) = Future{traversable.aggregate(z)(seqop,combop)}
  def apply(idx:Long) = Future{traversable.apply(idx)}
  def collectFirst[B](pf: PartialFunction[A, B]) = Future{traversable.collectFirst(pf)}
  def containsSlice[B](that: Seq[B]): Future[Boolean] = containsSlice(that,0)
  def containsSlice[B](that: Seq[B], start: Long): Future[Boolean] = Future{traversable.containsSlice(that,start)}
  def corresponds[B](that: Seq[B])(p: (A, B) => Boolean) = Future{traversable.corresponds(that)(p)}
  def corresponds[B](that: LongTraversable[B])(p: (A, B) => Boolean) = Future{traversable.corresponds(that)(p)}
  def count(p: A => Boolean) = Future{traversable.count(p)}
  def exists(p: A => Boolean) = Future{traversable.exists(p)}
  def find(p: A => Boolean) = Future{traversable.find(p)}
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1) = Future{traversable.fold(z)(op)}
  def foldLeft[B](z: B)(op: (B, A) => B) = Future{traversable.foldLeft(z)(op)}
  def foldRight[B](z: B)(op: (A, B) => B) = Future{traversable.foldRight(z)(op)}
  def forall(p: A => Boolean) = Future{traversable.forall(p)}
  def foreach[U](f: A => U):Future[Unit] = Future {traversable.foreach(f)}
  def groupBy[K](f: A => K) = Future{traversable.groupBy(f)}
  def head:Future[A] = Future{traversable.head}
  def headOption:Future[Option[A]] = Future{traversable.headOption}
  def indexOf[B >: A](elem: B) = Future{traversable.indexOf(elem)}
  def indexOf[B >: A](elem: B, from: Long) = Future{traversable.indexOf(elem,from)}
  def isDefinedAt(idx: Long) = Future{traversable.isDefinedAt(idx)}
  def isEmpty = Future{traversable.isEmpty}
  def last = Future{traversable.last}
  def lastOption = Future{traversable.lastOption}
  def lastIndexOf[B >: A](elem: B) = Future{traversable.lastIndexOf(elem)}
  def lastIndexOf[B >: A](elem: B, end: Long) = Future{traversable.lastIndexOf(elem,end)}
  def lastIndexWhere(p: A => Boolean) = Future{traversable.lastIndexWhere(p)}
  def lastIndexWhere(p: A => Boolean, end: Long) = Future{traversable.lastIndexWhere(p, end)}
  def lcount(p: A => Boolean) = Future{traversable.lcount(p)}
  def limitFold[U](init: U)(op: (U, A) => FoldResult[U]) = Future{traversable.limitFold(init)(op)}
  def lsize = Future{traversable.lsize}
  def max[B >: A](implicit cmp: Ordering[B]) = Future{traversable.max(cmp)}
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]) = Future{traversable.maxBy(f)(cmp)}
  def min[B >: A](implicit cmp: Ordering[B]) = Future{traversable.min(cmp)}
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]) = Future{traversable.minBy(f)(cmp)}
  def mkString(start: String, sep: String, end: String) = Future{traversable.mkString(start,sep,end)}
  def mkString(sep: String) = Future{traversable.mkString(sep)}
  def mkString = Future{traversable.mkString}
  def nonEmpty = Future{traversable.nonEmpty}
  def prefixLength(p: A => Boolean) = Future{traversable.prefixLength(p)}
  def product[B >: A](implicit num: Numeric[B]) = Future{traversable.product(num)}
  def reduce[A1 >: A](op: (A1, A1) => A1) = Future{traversable.reduce(op)}
  def reduceOption[A1 >: A](op: (A1, A1) => A1) = Future{traversable.reduceOption(op)}
  def reduceLeft[B >: A](op: (B, A) => B) = Future{traversable.reduceLeft(op)}
  def reduceLeftOption[B >: A](op: (B, A) => B) = Future{traversable.reduceLeftOption(op)}
  def reduceRight[B >: A](op: (A, B) => B) = Future{traversable.reduceRight(op)}
  def reduceRightOption[B >: A](op: (A, B) => B) = Future{traversable.reduceRightOption(op)}
  def sameElements[B >: A](that: Iterable[B]) = Future{traversable.sameElements(that)}
  def sameElements[B >: A](that: LongTraversable[B]) = Future{traversable.sameElements(that)}
  def segmentLength(p: (A) => Boolean, from: Long = 0) = Future{traversable.segmentLength(p,from)}
  def size = Future{traversable.size}
  def startsWith[B](that: LongTraversable[B]) = Future{traversable.startsWith(that)}
  def startsWith[B](that: LongTraversable[B], offset: Long) = Future{traversable.startsWith(that,offset)}
  def startsWith[B](that: Seq[B], offset: Long) = Future{traversable.startsWith(that,offset)}
  def startsWith[B](that: Seq[B]) = Future{traversable.startsWith(that)}
  def sum[B >: A](implicit num: Numeric[B]) = Future{traversable.sum(num)}
  def toIndexedSeq[B >: A] = Future{traversable.toIndexedSeq}
  def toList = Future{traversable.toList}
  def toSeq = Future{traversable.toSeq}
}
