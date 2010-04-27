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

trait StreamLongTraversable[A] extends LongTraversable[A]
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
      var c = 0
      while(v != None && c < end) {
        v foreach {x => 
          c += 1
          conv(x) foreach f
        }
        v = source.read(stream)
      }
    }
  }

  override def ldrop(length : Long) : LongTraversable[A] = new StreamLongTraversable[A] {
    type In = self.In
    type SourceOut = self.SourceOut
    val source = self.source
    def conv = self.conv
    val start = self.start + length
    val end = self.end
  }
  override def drop(length : Int) = ldrop(length.toLong)

  override def ltake(length : Long) : LongTraversable[A] = new StreamLongTraversable[A] {
    type In = self.In
    type SourceOut = self.SourceOut
    def source = self.source
    def conv = self.conv
    def start = self.start + length
    def end = length
  }
  override def take(length : Int) = ltake(length.toLong)

   override def view = new StreamLongTraversableView[A, LongTraversable[A]] {
      protected lazy val underlying = self.repr

      type In = self.In
      type SourceOut = self.SourceOut
      def source = self.source
      def conv = self.conv
      def start = self.start
      def end = self.end
    }
   override def view(from: Int, until: Int) = view.slice(from, until); 
   
//  def lview(from: Long, until: Long) = view.lslice(from, until); 
}

object StreamLongTraversable {
  def apply[A](pin : Resource[InputStream], pconv : Int => Traversable[A], pstart : Long = 0, pend : Long = Long.MaxValue) = {
    new StreamLongTraversable[A] {
      type In = InputStream
      type SourceOut = Int
      
      def source = new TraversableSource[InputStream, Int] {
        def resource = pin
        def skip(stream:InputStream, count:Long) = stream.skip(count)
        def read(stream:InputStream) = stream.read match {
          case -1 => None
          case i => Some(i)
        }
      }
      
      def conv = pconv
      def start = pstart
      def end = pend
    }
  }
  
}