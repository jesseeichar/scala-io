package scalax.io

import java.io.Closeable
import resource.{AbstractManagedResource, ManagedResourceOperations}
import collection.{GenTraversableOnce, Iterator, TraversableOnce}
import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.SyncVar

trait CloseableIterator[+A] extends Iterator[A] with Closeable {
  self =>
  val creationPoint = new Exception();

  @specialized(Byte, Int, Float, Double, Long)
  def next(): A
  
  def hasNext: Boolean
  
  final def close() {
    doClose()
  }

  protected def doClose():Unit

  class Proxy[+B,C <: Iterator[B]](protected val wrapped:C,otherComposingIterators:CloseableIterator[_]*) extends CloseableIterator[B] {
    @specialized(Byte,Int,Float,Double,Long)
    final def next() = wrapped.next
    final def hasNext: Boolean = wrapped.hasNext
    def doClose() = {
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
  override def flatMap[B](f: (A) => GenTraversableOnce[B]) = Proxy(super.flatMap(f))
  override def dropWhile(p: (A) => Boolean) = Proxy(super.dropWhile(p))
  override def filter(p: (A) => Boolean) = Proxy(super.filter(p))
  override def takeWhile(p: A => Boolean) = Proxy(super.takeWhile(p))
  override def ++[B >: A](that: => GenTraversableOnce[B]): CloseableIterator[B] =
    new Proxy[B,Iterator[B]](super.++(that)) {
      override def doClose() = {
        self.close()
        that match {
          case that:Closeable => that.close
          case _ => ()
        }
      }
    }

  def lslice(from:Long,until:Long) = Proxy[A](new Iterator[A]{
      private var count = from
      private val iter = {
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
    @specialized(Byte,Int,Float,Double,Long)
    final def next(): A = iter.next
    final def hasNext: Boolean = iter.hasNext
    def doClose() {}
  }
  def selfClosing[A](wrapped: CloseableIterator[A]) = new CloseableIterator[A] {
    @specialized(Byte,Int,Float,Double,Long,Char)
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