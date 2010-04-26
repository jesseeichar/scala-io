/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.resource

import scalax.io.{
  LongTraversableLike, LongTraversableViewLike, LongTraversable
}
import scala.collection._
import scala.collection.generic._

import java.io.{
  InputStream, Reader, Closeable
}


trait StreamLongTraversable[A] extends LongTraversable[A]
                             with LongTraversableLike[A, LongTraversable[A]] {
  self => 



  protected[resource] trait TraversableSource {
    def skip(stream:InputStream, count:Long) : Unit
    def read(stream:InputStream) : Option[Traversable[A]]
  }

  type InputSource = InputStream
  protected def in : Resource[InputStream]
  protected def conv : Int => Traversable[A]
  
  def source : TraversableSource = new TraversableSource{
    def skip(stream:InputStream, count:Long) = stream.skip(count)
    def read(stream:InputStream) = stream.read match {
      case -1 => None
      case i => Some(conv(i))
    }
  }

  protected def start : Long
  protected def end : Long

  def foreach[U](f: (A) => U) : Unit = doForeach(f)
  def doForeach[U](f: A => U) : Unit = {
    for(stream <- in) {
      if(start > 0) source.skip(stream,start)

      var v = source.read(stream)
      var c = 0
      while(v != None && c < end) {
        v.flatten foreach {x => c += 1 ; f(x)}
        v = source.read(stream)
      }
    }
  }
  
  override def ldrop(length : Long) = StreamLongTraversable(in, conv, start + length, end)
  override def drop(length : Int) = StreamLongTraversable(in, conv, start + length, end)

  override def ltake(length : Long) = StreamLongTraversable(in, conv, start, length)
  override def take(length : Int) = StreamLongTraversable(in, conv, start, length)
  
   override def view = new StreamLongTraversableView[A, LongTraversable[A]] {
      protected lazy val underlying = self.repr
      val in = self.in
      val conv = self.conv
      val start = self.start
      val end = self.end
    }
   override def view(from: Int, until: Int) = view.slice(from, until); 
//  def lview(from: Long, until: Long) = view.lslice(from, until); 
}

object StreamLongTraversable {
  def apply[A](pin : => Resource[InputStream], pconv : Int => Traversable[A], pstart : Long = 0, pend : Long = Long.MaxValue) = {
    new StreamLongTraversable[A] {
      override def in = pin
      override def conv = pconv
      override def start = pstart
      override def end = pend
    }
  }
}