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
 * An object for reading and writing to Random Access IO objects such as Files.  This is analogous to the Java 7 NIO2
 * SeekableByteChannel.  In large part this is as an abstraction away from FileChannel allowing other implementations.
 *
 * @author Jesse Eichar
 * @since 1.0
 */
trait SeekableByteChannel extends ByteChannel
                             with Channel
                             with Closeable
                             with ReadableByteChannel
                             with WritableByteChannel {
  /**
   * Returns this channel's position
   * @return This channel's position. A non-negative integer counting the number of bytes from the
   *         beginning of the entity to the current position
   */
  def position : Long

  /**
   * Sets this channel's position.
   *
   * No exception is thrown if value is greater than the length of the Channel but a read will fail and a write will
   * grow the channel to ''position'' and begin writing there.
   *
   * @param newPosition The new position of the channel
   * @return the same channel object
   */
  def position(newPosition : Long) : SeekableByteChannel

  def read(dst : ByteBuffer) : Int
  def read(dst : ByteBuffer, pos:Long) : Int = doAt (pos){read(dst)}

  /**
   * Return current size of Channel
   */
  def size : Long

  /**
   * Reduce the size of the channel to the indicated size.  If size is > than size of channel then channel is not
   * modified
   *
   * @param size max size for channel after operation
   * @return the same channel object
   */
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
