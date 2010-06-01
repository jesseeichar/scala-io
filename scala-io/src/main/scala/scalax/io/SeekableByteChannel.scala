/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io

import java.io.Closeable
import java.nio.ByteBuffer
import java.nio.channels.{
  ByteChannel, Channel, ReadableByteChannel, WritableByteChannel
}

/**
 * An object for reading and writing to Random Access IO objects such as Files.
 *
 * @author Jesse Eichar
 * @since 1.0
 */
trait SeekableByteChannel extends ByteChannel
                             with Channel 
                             with Closeable 
                             with ReadableByteChannel 
                             with WritableByteChannel {
  def position : Long
  def position(newPosition : Long) : SeekableByteChannel
  def read(dst : ByteBuffer) : Int
  def read(dst : ByteBuffer, pos:Long) : Int = doAt (pos){read(dst)}
  def size : Long
  def truncate(size : Long) : SeekableByteChannel
  def write(src : ByteBuffer) : Int
  def write(src : ByteBuffer, pos:Long) : Int = doAt (pos){write(src)}
  
  private def doAt[T](pos:Long)(f: => T) = {
    val mark = position
    position(pos)
    val result = f
    position(mark)
    result
  }
}