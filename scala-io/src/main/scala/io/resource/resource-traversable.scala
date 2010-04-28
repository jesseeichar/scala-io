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


protected[resource] trait TraversableSource[In <: java.io.Closeable, A] {
  def resource : Resource[In]
  def skip(stream:In, count:Long) : Unit
  def read(stream:In) : Option[A]
}

trait ResourceTraversable[A] extends LongTraversable[A]
                             with LongTraversableLike[A, LongTraversable[A]] {
  self =>

  type In <: java.io.Closeable
  type SourceOut
  
  def source : TraversableSource[In,SourceOut]

  protected def conv : SourceOut => Traversable[A]
  protected def start : Long
  protected def end : Long

  def foreach[U](f: (A) => U) : Unit = doForeach(f)
  def doForeach[U](f: A => U) : Unit = {
    for(stream <- source.resource) {
      if(start > 0) source.skip(stream,start)

      var v = source.read(stream)
      var c = start
      val funAndInc = f andThen {f => c += 1}
      while(v != None && c < end) {
        conv(v.get) foreach funAndInc
        v = source.read(stream)
      }
    }
  }

  override def ldrop(length : Long) : LongTraversable[A] = lslice(length,Long.MaxValue)
  override def drop(length : Int) = ldrop(length.toLong)

  override def ltake(length : Long) = lslice(0, length)
  override def take(length : Int) = ltake(length.toLong)

  override def lslice(_start : Long, _end : Long) = copy(_start = start + (0L max _start), _end = (_end min end))
  override def slice(_start : Int, _end : Int) = lslice(_start.toLong,_end.toLong)

   override def view = new ResourceTraversableView[A, LongTraversable[A]] {
      protected lazy val underlying = self.repr

      type In = self.In
      type SourceOut = self.SourceOut
      def source = self.source
      def conv = self.conv
      def start = self.start
      def end = self.end
    }
   override def view(from: Int, until: Int) = view.slice(from, until);
   
   private def copy[B](_conv : this.SourceOut => Traversable[B] = conv, _start : Long = start, _end : Long = end) : LongTraversable[B] = {
     new ResourceTraversable[B] {
       type In = self.In
       type SourceOut = self.SourceOut
       def source = self.source
       def conv = _conv
       def start = _start
       def end = _end
     }
   }
}

object ResourceTraversable {
  def apply[A](_in : Resource[InputStream], _conv : Int => Traversable[A] = (i:Int) => List(i), _start : Long = 0, _end : Long = Long.MaxValue) = {
    new ResourceTraversable[A] {
      type In = InputStream
      type SourceOut = Int
      
      def source = new TraversableSource[InputStream, Int] {
        def resource = _in
        def skip(stream:InputStream, count:Long) = stream.skip(count)
        def read(stream:InputStream) = stream.read match {
          case -1 => None
          case i => Some(i)
        }
      }
      
      def conv = _conv
      def start = _start
      def end = _end
    }
  }
  
}