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
import java.nio.{ByteBuffer => NioByteBuffer}
import java.nio.channels.ReadableByteChannel

/**
 * A way of abstracting over the source Resource's type
 *
 * @see [[ResourceTraversable]]
 */
protected[io] trait TraversableSource[In <: Closeable, A] {
  def close(): List[Throwable]
  def skip(count:Long) : Unit
  def read() : Iterator[A]
  def initializePosition(pos:Long) : Unit
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

  /**
   * returns the strategy that permits
   */
  def source : TraversableSource[In,SourceOut]

  protected def conv : SourceOut => A
  protected def start : Long
  protected def end : Long


  protected[io] def iterator: CloseableIterator[A] = getIterator

  protected def getIterator = new CloseableIterator[A] {

    private val openedSource = source
    openedSource.skip(start)

    var nextEl:Iterator[SourceOut] = null
    var c = start

    def next(): A = {
      val n = nextEl.next()
      c += 1

      if(!nextEl.hasNext) {
        nextEl = null
      }
      conv(n)
    }

    def doHasNext: Boolean = {
      if(c >= end) return false

      if(nextEl == null) {
        nextEl = openedSource.read()
      }
      nextEl.nonEmpty
    }

    def doClose() = source.close()
  }

  override def isEmpty: Boolean = CloseableIterator.managed(iterator).acquireAndGet(_.isEmpty)

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
  def streamBased[A,B](opener : => OpenedResource[InputStream],
                     bufferFactory : => Array[Byte] = new Array[Byte](Constants.BufferSize),
                     parser : (Array[Byte],Int) => Iterator[A] = (a:Array[Byte],length:Int) => a.take(length).iterator,
                     initialConv: A => B = (a:A) => a,
                     startIndex : Long = 0,
                     endIndex : Long = Long.MaxValue) = {
    new ResourceTraversable[B] {
      type In = InputStream
      type SourceOut = A

      def source = new TraversableSource[InputStream, A] {
        val buffer = bufferFactory

        val openedResource = opener
        val stream = openedResource.get
        def read() = {
          val read = stream.read (buffer)
          parser(buffer,read)
        }

        def initializePosition(pos: Long) = skip(pos)

        def skip(count: Long) = stream.skip(count)

        def close(): List[Throwable] = openedResource.close()
      }

      protected val conv = initialConv
      protected val start = startIndex
      protected val end = endIndex
    }
  }

  def readerBased[A](opener : => OpenedResource[Reader],
                  initialConv: Char => A = (a:Char) => a,
                  startIndex : Long = 0,
                  endIndex : Long = Long.MaxValue) = {
    new ResourceTraversable[A] {
      type In = Reader
      type SourceOut = Char

      def source = new TraversableSource[Reader, Char] {
        val openedResource = opener
        def close(): List[Throwable] = openedResource.close()
        def initializePosition(pos: Long) = skip(pos)
        def skip(count:Long) = openedResource.get.skip(count)
        def read() = openedResource.get.read match {
          case -1 => Iterator.empty
          case i => Iterator.single(i.toChar)
        }
      }

      protected val conv = initialConv
      protected val start = startIndex
      protected val end = endIndex
    }
  }

  def byteChannelBased[A,B](opener : => OpenedResource[ReadableByteChannel],
                          byteBufferFactory : => NioByteBuffer = NioByteBuffer.allocate(Constants.BufferSize),
                          parser : NioByteBuffer => Iterator[A] = (c:NioByteBuffer) => JavaConversions.byteBufferToTraversable(c).iterator,
                          initialConv: A => B = (a:A) => a,
                          startIndex : Long = 0,
                          endIndex : Long = Long.MaxValue) = {
    new ResourceTraversable[B] {
      type In = ReadableByteChannel
      type SourceOut = A

      def source = new TraversableSource[ReadableByteChannel, A] {
        val buffer = byteBufferFactory

        val openedResource = opener
        val channel = openedResource.get
        def read() = {
          buffer.clear
          channel read buffer
          buffer.flip
          parser(buffer)
        }

        def initializePosition(pos: Long) = skip(pos)

        def skip(count: Long) {
          if(count > 0) {
            val toRead = (buffer.capacity.toLong min count).toInt

            buffer.clear()
            buffer.limit(toRead)

            val read = channel read buffer
            if(read > -1) skip(count - read)
          }
        }

        def close(): List[Throwable] = openedResource.close()
      }

      protected val conv = initialConv
      protected val start = startIndex
      protected val end = endIndex
    }
  }
  def seekableByteChannelBased[A,B](opener : => OpenedResource[SeekableByteChannel],
                          byteBufferFactory : => NioByteBuffer = NioByteBuffer.allocate(Constants.BufferSize),
                          parser : NioByteBuffer => Iterator[A] = (c:NioByteBuffer) => JavaConversions.byteBufferToTraversable(c).iterator,
                          initialConv: A => B = (a:A) => a,
                          startIndex : Long = 0,
                          endIndex : Long = Long.MaxValue) = {
    new ResourceTraversable[B] {
      type In = SeekableByteChannel
      type SourceOut = A

      def source = new TraversableSource[SeekableByteChannel, A] {
        val buffer = byteBufferFactory

        val openedResource = opener
        val channel = openedResource.get

        var position = 0L

        def read() = {
          channel.position(position)
          buffer.clear
          position += (channel read buffer)
          buffer.flip
          parser(buffer)
        }

        def initializePosition(pos: Long) = {
          position = pos
          channel.position(pos)
        }

        def skip(count: Long) {
          if(count > 0) {
            position += count
            channel.position(position)
          }
        }

        def close(): List[Throwable] = openedResource.close()
      }

      protected val conv = initialConv
      protected val start = startIndex
      protected val end = endIndex
    }
  }

  val toIntConv : Byte => Int = (b:Byte) => {
    if(b < 0) 256 + b
    else b.toInt
  }

}
