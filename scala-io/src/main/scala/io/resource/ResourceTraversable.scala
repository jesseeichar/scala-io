/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io.LongTraversable
import scala.collection._
import scala.collection.generic._

import java.io.{
  InputStream, Reader, Closeable
}

object Alias {
type ReadableSkippable = {
    def read() : Int
    def skip(l : Long) : Long
  }
}

/**
 * A stream based LongTraversable
 *
 */
protected[io] class ResourceTraversable[R <: Closeable with Alias.ReadableSkippable,A](resource : Resource[R], conv : Int => A, start : Long = 0, end : Long = Long.MaxValue ) 
                  extends LongTraversable[A]
                     with GenericTraversableTemplate[A,LongTraversable] {
                    
  override def companion = ResourceTraversable

  def foreach[U](f: A => U): Unit = {
    for (r <- resource) {
      r skip start
      var i = r.read
      var count = 0L
      while (i != -1 && count < end) {
        f (conv(i))
        count += 1
        i = r.read
      }
    }
  }
  
  override def take(i : Int) = new ResourceTraversable[R,A](resource, conv, start, i.toLong.min(end))
  override def ltake(i : Long) = new ResourceTraversable[R,A](resource, conv, start, i.min(end))
  override def drop(i : Int) = new ResourceTraversable[R,A](resource, conv, start + i, end)
  override def ldrop(i : Long) = new ResourceTraversable[R,A](resource, conv, start + i, end)

}

object ResourceTraversable extends TraversableFactory[LongTraversable] {  
  
  def apply[I <: InputStream](resource : InputStreamResource[I]) = new ResourceTraversable[InputStream, Int](resource, i => i)
  def apply[R <: Reader](resource : ReaderResource[R]) = new ResourceTraversable[Reader, Char](resource, i => i.toChar)
  
  implicit def canBuildFrom[R, A]: CanBuildFrom[Coll, A, LongTraversable[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A] = new scala.collection.mutable.LazyBuilder[A,LongTraversable[A]] {
    def result = {
      val data = parts.foldLeft(List[A]()){(l,n) => l ++ n}
      new ResourceTraversable(null, null)
    }
  }
}