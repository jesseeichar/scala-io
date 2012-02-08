/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2011, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scalax.io
import java.io.InputStream
import java.nio.channels.ReadableByteChannel
import java.nio.ByteBuffer
import java.io.Closeable

/**
 * Not public API.  I don't think I like the idea of having constants
 * defined like this.  At the very least there needs to be a way to
 * override the default values.
 */
object Buffers {
/*  final val BufferSize = 4 * 1024
  final val CharBufferSize = 1024

  def arrayBuffer(size: Option[Long]) = {
    size match {
      case Some(size) => new Array[Byte](bufferSize(size, 0))
      case _ => new Array[Byte](BufferSize)
    }
  }
  def nioByteBuffer(size: Option[Long]) = {
    size match {
      case Some(size) => ByteBuffer.allocate(bufferSize(size, 0))
      case _ => ByteBuffer.allocate(BufferSize)
    }

  }
  def nioDirectBuffer(size: Option[Long]) = {
    size match {
      case Some(size) => ByteBuffer.allocateDirect(bufferSize(size, 0))
      case _ => ByteBuffer.allocateDirect(BufferSize)
    }
  }
  def byteBuffer(c: ReadableByteChannel): ByteBuffer = c match {
    case s: SeekableByteChannel =>
      byteBuffer(s.size)
    case _ => ByteBuffer.allocate(BufferSize)
  }
  def byteBuffer(size: Long, min: Int = 0): ByteBuffer = {
    ByteBuffer.allocate(bufferSize(size, min))
  }
  def readerBuffer = new Array[Char](CharBufferSize)
  def bufferSize(size: Long, min: Int) = {
    if (size < BufferSize && size > min) size.toInt
    else BufferSize
  }*/
}
