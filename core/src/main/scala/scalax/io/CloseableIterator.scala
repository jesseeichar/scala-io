package scalax.io

import java.io.Closeable
import resource.{AbstractManagedResource, ManagedResourceOperations}
import collection.{GenTraversableOnce, Iterator, TraversableOnce}
import java.util.concurrent.locks.ReentrantLock
import scala.util.control.Exception
import Exception.allCatch


trait CloseableIterator[@specialized(Byte,Char) +A] extends Iterator[A] {
  self =>
  def next(): A
  def hasNext: Boolean
  protected def doClose():List[Throwable]

  override def foreach[@specialized(Unit) U](f: A => U) =
    while (hasNext) f(next)

  final def close():List[Throwable] = doClose()

  override def take(i: Int) = lslice(0, i)
  def ltake(i: Long) = lslice(0, i)
  def ldrop(i: Long) = lslice(i,Long.MaxValue)
  def lslice(from: Long, until: Long):CloseableIterator[A] = {
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
private[io] class CloseableIteratorOps[+A](val iter: CloseableIterator[A]) {
  def collect[B, That](pf: PartialFunction[A, B]) = Proxy(iter.collect(pf))
  def map[B](f: A => B) = Proxy(iter.map(f))
  def flatMap[B](f: (A) => GenTraversableOnce[B]) = Proxy(iter.flatMap(f))
  def dropWhile(p: (A) => Boolean) = Proxy(iter.dropWhile(p))
  def filter(p: (A) => Boolean) = Proxy(iter.filter(p))
  def filterNot(p: (A) => Boolean) = Proxy(iter.filterNot(p))
  def takeWhile(p: A => Boolean) = Proxy(iter.takeWhile(p))
  def init = new InitIterator(iter)
  def ++[B >: A](that: => GenTraversableOnce[B]): CloseableIterator[B] = {
    val concThat = that
    new Proxy[B,Iterator[B]](iter.++(concThat)) {
      override def doClose() = {
        iter.close() ++ CloseableIterator.safeClose(concThat)
      }
    }
  }

  def modifiedSliding(size: Int, step: Int): CloseableIterator[Seq[A]] = Proxy(iter.sliding(size,step))
  def zipAll[B, A1 >: A, B1 >: B](that: Iterator[B], thisElem: A1, thatElem: B1): CloseableIterator[(A1, B1)] =
    Proxy(iter.zipAll(that,thisElem,thatElem))
  def zipWithIndex: CloseableIterator[(A, Int)] = {
    val zipped = iter.zipWithIndex
    new Proxy[(A, Int),Iterator[(A, Int)]](zipped)
  }
  def zip[B](that: Iterator[B]): CloseableIterator[(A, B)] =
    Proxy(iter.zip(that))

  def take(i: Int) = iter.lslice(0, i)
  def drop(i: Int) = iter.lslice(i, Long.MaxValue)
  def lslice(from: Long, until: Long) = iter.lslice(from,until)
  import CloseableIterator.safeClose
  class Proxy[+B, C <: Iterator[B]](private[this] val wrapped: C, otherComposingIterators: Iterator[_]*) extends CloseableIterator[B] {
    final def next() = wrapped.next
    final def hasNext: Boolean = wrapped.hasNext
    def doClose():List[Throwable] = {
      safeClose(iter) ++ otherComposingIterators.flatMap(safeClose)
    }
  }
  object Proxy {
    def apply[B](wrapped: Iterator[B]): CloseableIterator[B] = new Proxy[B, Iterator[B]](wrapped)
  }
}

object CloseableIterator {
  def withIterator[A,U](iterator: => CloseableIterator[A], context:ResourceContext)(f:CloseableIterator[A] => U): U = {
    val resourceEither = allCatch.either { iterator }
    var closeExceptions: List[Throwable] = Nil

    /** Close resource and assign any exceptions to closeException */
    def close(resource:CloseableIterator[A]) = try {
      closeExceptions = resource.close()
    } catch {
      case t: Throwable => closeExceptions = List(t)
    }

    /** Handle error that occurs during resource access */
    def handleAccessError: PartialFunction[Throwable, Either[Throwable, U]] = {
      case c: scala.util.control.ControlThrowable => throw c
      case t => Left(t)
    }

    resourceEither match {
      case Left(t) =>
        throw context.openErrorHandler(t)
      case Right(resource) =>
        val result =
          try Right(f(resource))
          catch handleAccessError
          finally close(resource)

        val handleError = result.left.toOption ++ closeExceptions nonEmpty

        val finalResult = if (handleError) {
            context.errorHandler(result, closeExceptions)
        } else {
          result.right.get
        }

        if (System.identityHashCode(finalResult) == System.identityHashCode(resource)) {
            throw new AssertionError("the iterator may not escape the bounds of this block")
        }
        finalResult
    }
  }
  private[io] def safeClose(iter: Any):List[Throwable] =
    iter match {
        case ci: CloseableIterator[_] =>
          ci.close()
        case c: Closeable =>
          Exception.allCatch.either{c.close()}.left.toOption.toList
        case _ => Nil
    }

  def apply[A](iter: Iterator[A]) = iter match {
    case closeable:CloseableIterator[A] => closeable
    case _ => new CloseableIterator[A] {
        final def next(): A = iter.next
        final def hasNext: Boolean = iter.hasNext
        def doClose() = Nil
      }
  }
  def empty[A] = apply(Iterator.empty)
  def selfClosing[A](wrapped: CloseableIterator[A]) = new CloseableIterator[A] {
    final def next(): A = wrapped.next
    final def hasNext: Boolean = {
      val next = wrapped.hasNext
      if(!next) close()
      next
    }
    def doClose = wrapped.close
  }
}

private[io] class InitIterator[@specialized(Byte) +A](iter:CloseableIterator[A]) extends CloseableIterator[A] {
    private[this] var nextEl:A = _

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
