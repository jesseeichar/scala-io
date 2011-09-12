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

/**
 * Not public API.  I don't think I like the idea of having constants
 * defined like this.  At the very least there needs to be a way to
 * override the default values.
 */
object Buffers {
  final val BufferSize = 8 * 1024
  final val CharBufferSize = 1024
  
  def inputStreamBuffer = new Array[Byte](BufferSize)
  def readerBuffer = new Array[Char](BufferSize)
  def byteBuffer(c:ReadableByteChannel):ByteBuffer = c match {
    case s:SeekableByteChannel => 
      byteBuffer(s.size)
    case _ => ByteBuffer.allocateDirect(BufferSize)
  }
  def byteBuffer(size:Long, min:Int = 0):ByteBuffer = { 
	  val finalSize = if(size < BufferSize && size > min) size.toInt
	  else BufferSize
	  ByteBuffer.allocateDirect(finalSize)
  }
	
}
