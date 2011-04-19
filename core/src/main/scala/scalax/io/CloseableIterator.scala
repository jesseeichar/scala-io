package scalax.io

import java.io.Closeable
import collection.{Iterator, TraversableOnce}
import resource.{AbstractManagedResource, ManagedResourceOperations}

trait CloseableIterator[+A] extends Iterator[A] with Closeable {
  self =>
  class Proxy[+B,C <: Iterator[B]](protected val wrapped:C,otherComposingIterators:CloseableIterator[_]*) extends CloseableIterator[B] {
    def next() = wrapped.next
    def hasNext: Boolean = wrapped.hasNext
    def close() = {
      safeClose(self)
      otherComposingIterators.foreach(safeClose)
    }
    private def safeClose(iter:Iterator[_]) = iter match {
      case ci:CloseableIterator[_] => ci.close()
      case _ => ()
    }
  }
  object Proxy {
    def apply[B](wrapped:Iterator[B]):CloseableIterator[B] = new Proxy[B,Iterator[B]](wrapped)
  }

  override def zipAll[B, A1 >: A, B1 >: B](that: Iterator[B], thisElem: A1, thatElem: B1): CloseableIterator[(A1, B1)] =
    Proxy(super.zipAll(that,thisElem,thatElem))
  override def zipWithIndex: CloseableIterator[(A, Int)] {var idx: Int} =
    new Proxy[(A, Int),Iterator[(A, Int)] {var idx: Int}](super.zipWithIndex) {
      var idx = wrapped.idx
    }
  override def zip[B](that: Iterator[B]): CloseableIterator[(A, B)] =
    Proxy(super.zip(that))

  override def map[B](f: (A) => B) = Proxy(super.map(f))
  override def flatMap[B](f: (A) => TraversableOnce[B]) = Proxy(super.flatMap(f))
  override def dropWhile(p: (A) => Boolean) = Proxy(super.dropWhile(p))
  override def filter(p: (A) => Boolean) = Proxy(super.filter(p))
  override def takeWhile(p: A => Boolean) = Proxy(super.takeWhile(p))
  override def ++[B >: A](that: => TraversableOnce[B]): CloseableIterator[B] =
    new Proxy[B,Iterator[B]](super.++(that)) {
      override def close() = {
        self.close()
        that match {
          case that:Closeable => that.close
          case _ => ()
        }
      }
    }
  def lslice(from:Long,until:Long) = Proxy[A](new Iterator[A]{
      var count = from
      val iter = {
        var toDrop = from
        var iter:Iterator[A] = self
        while(toDrop > 0) {
          if(toDrop > Integer.MAX_VALUE) {
            toDrop -= Integer.MAX_VALUE
            iter = iter.drop(Integer.MAX_VALUE)
          } else {
            iter = iter.drop(toDrop.toInt)
            toDrop = 0
          }
        }
        iter
      }

      def next(): A = {
        if(!hasNext)
          throw new NoSuchElementException("next on empty iterator")
        count += 1
        iter.next()
      }

      def hasNext: Boolean = {
        count < until && iter.hasNext
      }
  })

  def modifiedSliding(size: Int, step: Int): CloseableIterator[Seq[A]] = Proxy(super.sliding(size,step))
}

object CloseableIterator {
  def apply[A](iter:Iterator[A]) = new CloseableIterator[A]{
    def next(): A = iter.next
    def hasNext: Boolean = iter.hasNext
    def close() {}
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