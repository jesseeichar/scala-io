package scalax.io

import java.io.Closeable
import resource.{AbstractManagedResource, ManagedResourceOperations}
import collection.{GenTraversableOnce, Iterator, TraversableOnce}
import java.util.concurrent.locks.ReentrantLock
trait CloseableIterator[@specialized(Byte) +A] extends Iterator[A] with Closeable {
  self =>
  val creationPoint = new Exception();

  def next(): A
  def hasNext: Boolean
  protected def doClose(): Unit

  final override def foreach[@specialized(Unit) U](f: A => U) =
    while (hasNext) f(next)

  final def close() {
    doClose()
  }
  
  override def take(i: Int) = lslice(0, i)
  def lslice(from: Long, until: Long) = {
    var toDrop = from
    while (toDrop > 0) {
      if (toDrop > Int.MaxValue) {
        toDrop -= Int.MaxValue
        drop(Int.MaxValue)
      } else {
        drop(toDrop.toInt)
        toDrop = 0
      }
    }
    
    if (until == Long.MaxValue) this
    else new CloseableIterator[A] {
      private var count = from max 0
      def next(): A = {
        count += 1
        self.next()
      }

      def hasNext: Boolean = {
        count < until && self.hasNext
      }

      def doClose = self.close()
    }
  }
}

private[io] object CloseableIteratorOps { def apply[A](iter: CloseableIterator[A]) = new CloseableIteratorOps(iter) } 
private[io] class CloseableIteratorOps[A](iter: CloseableIterator[A]) {
  def map[B](f: A => B) = Proxy(iter.map(f))
  def flatMap[B](f: (A) => GenTraversableOnce[B]) = Proxy(iter.flatMap(f))
  def dropWhile(p: (A) => Boolean) = Proxy(iter.dropWhile(p))
  def filter(p: (A) => Boolean) = Proxy(iter.filter(p))
  def takeWhile(p: A => Boolean) = Proxy(iter.takeWhile(p))
  def init = new InitIterator(iter)
  def ++[B >: A](that: => GenTraversableOnce[B]): CloseableIterator[B] = {
    val concThat = that
    new Proxy[B,Iterator[B]](iter.++(concThat)) {
      override def doClose() = {
        iter.close()
        that match {
          case concThat:Closeable => concThat.close
          case _ => ()
        }
      }
    }
  }

  def modifiedSliding(size: Int, step: Int): CloseableIterator[Seq[A]] = Proxy(iter.sliding(size,step))
  def zipAll[B, A1 >: A, B1 >: B](that: Iterator[B], thisElem: A1, thatElem: B1): CloseableIterator[(A1, B1)] =
    Proxy(iter.zipAll(that,thisElem,thatElem))
  def zipWithIndex: CloseableIterator[(A, Int)] {var idx: Int} = {
    val zipped = iter.zipWithIndex
    new Proxy[(A, Int),Iterator[(A, Int)] {var idx: Int}](zipped) {
      var idx = zipped.idx
    }
  }
  def zip[B](that: Iterator[B]): CloseableIterator[(A, B)] =
    Proxy(iter.zip(that))

  def take(i: Int) = iter.lslice(0, i)
  def drop(i: Int) = iter.lslice(i, Long.MaxValue)
  def lslice(from: Long, until: Long) = iter.lslice(from,until)
  
  class Proxy[+B, C <: Iterator[B]](private[this] val wrapped: C, otherComposingIterators: Iterator[_]*) extends CloseableIterator[B] {
    final def next() = wrapped.next
    final def hasNext: Boolean = wrapped.hasNext
    def doClose() = {
      safeClose(iter)
      otherComposingIterators.foreach(safeClose)
    }
    private def safeClose(iter: Iterator[_]) = iter match {
      case ci: CloseableIterator[_] => ci.close()
      case _ => ()
    }
  }
  object Proxy {
    def apply[B](wrapped: Iterator[B]): CloseableIterator[B] = new Proxy[B, Iterator[B]](wrapped)
  }
}

object CloseableIterator {
  def apply[A](iter:Iterator[A]) = new CloseableIterator[A]{
    final def next(): A = iter.next
    final def hasNext: Boolean = iter.hasNext
    def doClose() {}
  }
  def selfClosing[A](wrapped: CloseableIterator[A]) = new CloseableIterator[A] {
    final def next(): A = wrapped.next
    final def hasNext: Boolean = {
      val next = wrapped.hasNext
      if(!next) close()
      next
    }
    def doClose = wrapped.close
  }
  implicit object ManagedResource extends resource.Resource[CloseableIterator[_]] {
    def close(r: CloseableIterator[_]) = r.close()
  }
  def managed[A](iter: => CloseableIterator[A]):ManagedResourceOperations[CloseableIterator[A]] =
    new AbstractManagedResource[CloseableIterator[A]] {
      protected def unsafeClose(handle: CloseableIterator[A]) = handle.close()
      protected def open: CloseableIterator[A] = iter
    }
}

private[io] class InitIterator[@specialized(Byte) A](iter:CloseableIterator[A]) extends CloseableIterator[A] {
    var nextEl:A = _

    if(iter.hasNext) {
      nextEl = iter.next()
    }
    def hasNext = {
      if(iter.hasNext) true
      else if (!iter.hasNext ) false
      else {
        nextEl = iter.next()
        iter.hasNext
      }
    }
    def next = {
      var tmp = nextEl
      nextEl = iter.next()
      tmp
    }
    
    def doClose = iter.close()
}