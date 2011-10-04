/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import support.Misc.safeSum
import java.io.Closeable
import java.io.InputStream
import java.io.Reader
import java.nio.channels.ReadableByteChannel
import java.nio.{ByteBuffer => NioByteBuffer}

import scala.Option.option2Iterable

import support.ArrayIterator
import support.NioByteBufferIterator

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
 * A Resource based implementation of a LongTraversable.  Optimized to only read the
 * required data from a stream
 */
private[io] trait ResourceTraversable[A] extends LongTraversable[A] {
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

    private var nextEl:Iterator[SourceOut] = null
    private var c = start

    final def next(): A = {
      val n = nextEl.next()
      c += 1

      if(!nextEl.hasNext) {
        nextEl = null
      }
      conv(n)
    }

    final def hasNext: Boolean = {
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

  private def copy[B](_conv: this.SourceOut => B = conv, _start: Long = start, _end: Long = end): LongTraversable[B] = {
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
  /**
   * The strategy defined for reading the input bytes and converting them
   * to the output type
   * 
   * @param A the type of object created by parsing the read byte array
   * @param I 
   */
  trait InputParser[+A,B] {
    /**
     * The type of iterator used by apply and created by iterator.
     */
    type I <: Iterator[A]
    /**
     * Create the initial iterator.  The returned iterator will be passed to the apply
     * and the iterator will not be shared between threads so it is acceptable to reuse
     * the same iterator to reduce the number of object creations.
     * 
     * @param input the object that collects the raw input data.  The object B should be mutable 
     * 		  and the contents should be updated, the reason for this is to have acceptable performance 
     * 		  during tight read loops 
     */
    def iterator(input:B):I
    /**
     * create the final iterator object to return.  Each call the same iterator object (created by calling iterator) is passed
     * to apply.   The iterator can be ignored if required but for performance it is recommended to reuse the same object as much as possible
     */
    def apply(iter:I,length:Int):I
  }
  val DefaultByteParser = new InputParser[Byte,Array[Byte]] {
    type I = ArrayIterator[Byte]
    def iterator(input:Array[Byte]) = new ArrayIterator(input,0)
    def apply(iter: ArrayIterator[Byte], length: Int) = {
      iter.start = 0
      iter.now = 0
      iter.end = length
      iter
    }
  }
  val IdentityByteConversion = identity[Byte]_
  def streamBased[A,B]
		  (opener : => OpenedResource[InputStream],
		   sizeFunc:() => Option[Long],
           parser : InputParser[A,Array[Byte]] = DefaultByteParser,
           initialConv: A => B = IdentityByteConversion,
           startIndex : Long = 0,
           endIndex: Long = Long.MaxValue) = {
    if (parser == DefaultByteParser && initialConv == IdentityByteConversion) {
      new traversable.ByteResourceTraversable(opener,sizeFunc, startIndex,endIndex).asInstanceOf[LongTraversable[B]]
    } else {
      new ResourceTraversable[B] {
        type In = InputStream
        type SourceOut = A

        def source = new TraversableSource[InputStream, A] {
          val buffer = Buffers.arrayBuffer(sizeFunc())
          val iter = parser.iterator(buffer)
          val openedResource = opener
          val stream = openedResource.get
          def read() = {
            val read = stream.read(buffer)
            parser(iter, read)
          }

          def initializePosition(pos: Long) = skip(pos)

          def skip(count: Long) = stream.skip(count)

          def close(): List[Throwable] = openedResource.close()
        }

        protected val conv = initialConv
        protected val start = startIndex
        protected val end = endIndex

        override def hasDefiniteSize = sizeFunc().nonEmpty
        override def lsize = sizeFunc() match {
          case Some(size) => size
          case None => super.lsize
        }
        override def size = lsize.toInt

      }
    }
  }
  val DefaultCharParser = new InputParser[Char,Array[Char]] {
    type I = ArrayIterator[Char]
    def iterator(input:Array[Char]) = new ArrayIterator(input,0)
    def apply(iter: ArrayIterator[Char], length: Int) = {
      iter.start = 0
      iter.now = 0
      iter.end = length
      iter
    }
  }
  val IdentityCharConversion = identity[Char]_
  def readerBased[A](opener : => OpenedResource[Reader],
				    parser : InputParser[Char,Array[Char]] = DefaultCharParser,
                    initialConv: Char => A = IdentityCharConversion,
                    startIndex : Long = 0,
                    endIndex: Long = Long.MaxValue) = {

    if (parser == DefaultCharParser && initialConv == IdentityCharConversion) {
    	new traversable.ReaderResourceTraversable(opener, startIndex, endIndex).asInstanceOf[LongTraversable[A]]
    } else {
      new ResourceTraversable[A] {
        type In = Reader
        type SourceOut = Char

        def source = new TraversableSource[Reader, Char] {
          val buffer = Buffers.readerBuffer
          val iter = parser.iterator(buffer)
          val openedResource = opener
          val stream = openedResource.get
          def read() = {
            val read = stream.read(buffer)
            parser(iter, read)
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
  }

  val DefaultByteBufferParser = new InputParser[Byte,NioByteBuffer] {
    type I = NioByteBufferIterator
    def iterator(input:NioByteBuffer) = new NioByteBufferIterator(input)
    def apply(iter: NioByteBufferIterator, length: Int) = iter
  }
    
  def byteChannelBased[A,B](opener : => OpenedResource[ReadableByteChannel],
		  					sizeFunc:() => Option[Long],
                            parser : InputParser[A,NioByteBuffer] = DefaultByteBufferParser,
                            initialConv: A => B = IdentityByteConversion,
                            startIndex : Long = 0,
                            endIndex : Long = Long.MaxValue):LongTraversable[B] = {
    if (parser == DefaultByteBufferParser && initialConv == IdentityByteConversion) {
      new traversable.ByteResourceTraversable(opener, sizeFunc, startIndex, endIndex).asInstanceOf[LongTraversable[B]]
    } else {
      new ResourceTraversable[B] {
        type In = ReadableByteChannel
        type SourceOut = A

        def source = new TraversableSource[ReadableByteChannel, A] {
          private final val openedResource = opener
          private final val channel = openedResource.get
          private final val buffer = Buffers.nioDirectBuffer(sizeFunc())
          private final val iter = parser.iterator(buffer)
          
          def read() = {
            buffer.clear
            val read = channel read buffer
            buffer.flip
            parser(iter, read)
          }

          def initializePosition(pos: Long) = skip(pos)

          def skip(count: Long) {
            if (count > 0) {
              val toRead = (buffer.capacity.toLong min count).toInt

              buffer.clear()
              buffer.limit(toRead)

              val read = channel read buffer
              if (read > -1) skip(count - read)
            }
          }

          def close(): List[Throwable] = openedResource.close()
        }

        protected val conv = initialConv
        protected val start = startIndex
        protected val end = endIndex
        
        override lazy val hasDefiniteSize= sizeFunc().nonEmpty
        override def lsize = sizeFunc() match {
          case Some(size) => size
          case None => super.size
        }
        override def size = lsize.toInt

      }
    }
  }
  
  def seekableByteChannelBased[A,B](opener : => OpenedResource[SeekableByteChannel],
                          sizeFunc:() => Option[Long],
                          parser : InputParser[A,NioByteBuffer] = DefaultByteBufferParser,
                          initialConv: A => B = IdentityByteConversion,
                          startIndex : Long = 0,
                          endIndex : Long = Long.MaxValue):LongTraversable[B] = {
    if (parser == DefaultByteBufferParser && initialConv == IdentityByteConversion) {
      new traversable.ByteResourceTraversable(opener, sizeFunc, startIndex, endIndex).asInstanceOf[LongTraversable[B]]
    } else {
        new ResourceTraversable[B] {
          type In = SeekableByteChannel
          type SourceOut = A
    
          def source = new TraversableSource[SeekableByteChannel, A] {
            private final val openedResource = opener
            private final val channel = openedResource.get
            private final val buffer = Buffers.nioDirectBuffer(Some(channel.size))
            private final val iter = parser.iterator(buffer)
    
            var position = 0L
    
            @inline
            final def read() = {
              channel.position(position)
              buffer.clear
              position += (channel read buffer)
              buffer.flip
              parser(iter,0) // length is ignored so just pass 0
            }
            
            def initializePosition(pos: Long) = {
              position = pos
              channel.position(pos)
            }
    
            def skip(count: Long) {
              if (count > 0) {
                position += count
                channel.position(position)
              }
            }
    
            def close(): List[Throwable] = openedResource.close()
          }
    
          protected val conv = initialConv
          protected val start = startIndex
          protected val end = endIndex
          
          override lazy val hasDefiniteSize= sizeFunc().nonEmpty
          override def lsize = sizeFunc() match {
            case Some(size) => size
            case None => super.size
          }
          override def size = lsize.toInt
        }
      }
  }

  val toIntConv : Byte => Int = (b:Byte) => {
    if(b < 0) 256 + b
    else b.toInt
  }

}
