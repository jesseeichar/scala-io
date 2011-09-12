package scalax.io

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import java.io.IOException
import java.lang.Math.min
import collection.mutable.ArrayBuffer
import scalax.io.StandardOpenOption._

class ArrayBufferSeekableChannel(data:ArrayBuffer[Byte],
                                 openOptions: OpenOption*)
                                (forceDeleteAction: (ArrayBufferSeekableChannel)=>Unit,
                                 closeAction:(ArrayBufferSeekableChannel) => Unit) extends SeekableByteChannel {

  var closed = false
  var position = 0L

  if(openOptions contains Truncate) data.clear()
  if(openOptions contains Append) position = data.size

  override def write (src: java.nio.ByteBuffer):Int = {
    val traversable = new Traversable[Byte]() {
      def foreach[U](f: Byte => U): Unit = {
        (src.position until src.limit) foreach {i => f(src get i)}
      }

      override lazy val size = super.size
    }

    data.remove(position.toInt, min(traversable.size, data.size - position.toInt))
    data.insertAll(position.toInt, traversable)
    position += traversable.size

    src.position(src.limit)

    traversable.size
  }
  override def truncate (size: Long):scalax.io.SeekableByteChannel = {
    data.reduceToSize(size.toInt)
    position = min(data.size, size)
    this
  }
  override def size = data.size
  override def read (dst: java.nio.ByteBuffer):Int = {
    if(position.toInt < data.size) {
      val toRead = data.view.slice(position.toInt, min(position.toInt + dst.limit, data.size))
      toRead foreach {b => dst.put(b)}
      position += toRead.size
      toRead.size
    } else {
      -1
    }
  }
  override def position (newPosition: Long):ArrayBufferSeekableChannel = {
    // TODO maybe new exception?
    if(newPosition > Int.MaxValue) throw new IOException(newPosition + " is larger than a Ram file can be");
    position = newPosition.toInt
    this
  }

  override def close() = {
    closed = true
    if(openOptions contains DeleteOnClose) forceDeleteAction(this)
    closeAction(this)
  }
  override def isOpen() = !closed

}
