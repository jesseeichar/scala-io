/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.{
  InputStream, Reader, Closeable
}
import collection.Iterator

/**
 * A way of abstracting over the source Resource's type
 *
 * @see [[ResourceTraversable]]
 */
protected[io] trait TraversableSource[In <: Closeable, A] {
  def resource : Resource[In]
  def skip(stream:In, count:Long) : Unit
  def read(stream:In) : Option[A]
}

/**
 * A Resource based implementation of a TraversableLong.  Optimized to only read the
 * required data from a stream
 */
private[io] trait ResourceTraversable[A] extends LongTraversable[A]
                             with LongTraversableLike[A, LongTraversable[A]] {
  self =>

  type In <: java.io.Closeable
  type SourceOut

  def source : TraversableSource[In,SourceOut]

  protected def conv : SourceOut => A
  protected def start : Long
  protected def end : Long


  protected def iterator: CloseableIterator[A] = getIterator

  protected def getIterator = new CloseableIterator[A] {

    private val resource = source.resource.open()
    var nextEl:Option[SourceOut] = _
    var c = start

    def next(): A = {
      val n = nextEl.get
      nextEl = null
      conv(n)
    }

    def hasNext: Boolean = {
      if(c >= end) return false

      if(nextEl == null) {
        nextEl = source.read(resource)
        c += 1
      }
      nextEl.isDefined
    }

    def close() = resource.close
  }

  /*
  override def foreach[U](f: (A) => U) : Unit = doForeach(f)
  protected def doForeach[U](f: A => U) : Unit = {
    for(stream <- source.resource) {
      if(start > 0) source.skip(stream,start)

      var v = source.read(stream)
      var c = start
      val funAndInc = conv andThen f andThen {f => c += 1}
      while(v != None && c < end) {
        funAndInc(v.get)
        v = source.read(stream)
      }
    }
  }
  protected def doForeach[U](pred:A => Boolean, f: A => U) : Unit = {

    for(stream <- source.resource) {
      if(start > 0) source.skip(stream,start)

      var v = source.read(stream)
      var c = start
      val funAndInc = conv andThen f andThen {f => c += 1}
      val continue = conv andThen pred
      while(v != None && c < end && continue(v.get)) {
        funAndInc(v.get)
        v = source.read(stream)
      }
    }
  }
  */

  override def ldrop(length : Long) : LongTraversable[A] = lslice(length,Long.MaxValue)
  override def drop(length : Int) = ldrop(length.toLong)

  override def ltake(length : Long) = lslice(0, length)
  override def take(length : Int) = ltake(length.toLong)

  override def lslice(_start : Long, _end : Long) = {
    val newStart = self.start + (_start max 0)
    val newEnd = if (_end > self.end) self.end
                 else safeSum(self.start,(_end max 0))

    copy(_start = newStart, _end = newEnd)
  }
  override def slice(_start : Int, _end : Int) = lslice(_start.toLong,_end.toLong)

  // make sure that when adding 2 number it doesn't overflow to a lower number
  protected def safeSum(numbers : Long*) = (0L /: numbers) { (next,acc) =>
      val sum = acc + next
      if(sum < acc) Long.MaxValue
      else sum
    }

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

   private def copy[B](_conv : this.SourceOut => B = conv, _start : Long = start, _end : Long = end) : LongTraversable[B] = {
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

private[io] object ResourceTraversable {
  def streamBased[A](_in : Resource[InputStream], _conv : Int => A = (i:Int) => i, _start : Long = 0, _end : Long = Long.MaxValue) = {
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
  def readerBased[A](_in : Resource[Reader], _conv : Char => A = (c:Char) => c, _start : Long = 0, _end : Long = Long.MaxValue) = {
    new ResourceTraversable[A] {
      type In = Reader
      type SourceOut = Char

      def source = new TraversableSource[Reader, Char] {
        def resource = _in
        def skip(reader:Reader, count:Long) =
          reader.skip(count)
        def read(reader:Reader) = reader.read match {
          case -1 => None
          case i => Some(i.toChar)
        }
      }

      def conv = _conv
      def start = _start
      def end = _end
    }
  }
  /*
  def byteChannelBased[A](_in : Resource[ReadableByteChannel],
                          _byteBuffer : => NioByteBuffer,
                          _conv : NioByteBuffer => A = (c:NioByteBuffer) => JavaConversions.byteBufferToTraversable(c) : Traversable[Byte],
                          _start : Long = 0, _end : Long = Long.MaxValue) = {
    new ResourceTraversable[A] {
      type In = ReadableByteChannel
      type SourceOut = NioByteBuffer

      def source = new TraversableSource[ReadableByteChannel, NioByteBuffer] {
        val buffer = _byteBuffer

        def resource = _in
        def skip(channel:ReadableByteChannel, count:Long) = {
          if(count > 0) {
            val toRead = buffer.capacity min count.toInt

            buffer.clear.limit(toRead)

            val read = channel read buffer
            if(read > -1) skip(channel, count - read)
          }
        }

        def read(channel:ReadableByteChannel) = {
          buffer.clear
          channel read buffer match {
            case -1 =>
              None
            case i =>
              buffer.flip
              Some(buffer)
          }
        }
      }

      def conv = _conv
      def start = _start
      def end = _end
    }
  }*/


}
