package scalax.io

import java.io.Closeable
import collection.{Iterator, TraversableOnce}

trait CloseableIterator[+A] extends Iterator[A] with Closeable {
  self =>
  class Proxy[+B](wrapped:Iterator[B]) extends CloseableIterator[B] {
    def next() = wrapped.next
    def hasNext: Boolean = wrapped.hasNext
    def close() = self.close
  }
  object Proxy {
    def apply[B](wrapped:Iterator[B]):CloseableIterator[B] = new Proxy(wrapped)
  }
  override def map[B](f: (A) => B) = Proxy(super.map(f))
  override def flatMap[B](f: (A) => TraversableOnce[B]) = Proxy(super.flatMap(f))
  override def dropWhile(p: (A) => Boolean) = Proxy(super.dropWhile(p))
  override def filter(p: (A) => Boolean) = Proxy(super.filter(p))
  override def takeWhile(p: A => Boolean) = Proxy(super.takeWhile(p))
  override def ++[B >: A](that: => TraversableOnce[B]): CloseableIterator[B] = new Proxy(super.++(that)) {
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
}