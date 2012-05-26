/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.nio

import scalax.io.SeekableByteChannel
import java.nio.{ByteBuffer => JByteBuffer}
import java.nio.channels.FileChannel
import java.nio.channels.ReadableByteChannel
import java.nio.channels.WritableByteChannel


class SeekableFileChannel(val self : FileChannel) extends SeekableByteChannel with Proxy {
  private[this] val wrapped = self
  def write (src: java.nio.ByteBuffer) = 
    wrapped.write(src)
  def truncate (size: Long)  = new SeekableFileChannel(wrapped.truncate(size))
  def size = wrapped.size
  def read (dst: java.nio.ByteBuffer) = wrapped.read(dst)
  def position (newPosition: Long) = new SeekableFileChannel(wrapped.position(newPosition))
  def position = wrapped.position
  def close = wrapped.close
  def isOpen = wrapped.isOpen
  def transferFrom(channel:ReadableByteChannel,position:Long,count:Long) =
    wrapped.transferFrom(channel,position,count)
    def transferTo(position:Long,count:Long, channel:WritableByteChannel) =
      wrapped.transferTo(position,count,channel)

  override def write(src : JByteBuffer, pos:Long) = wrapped.write(src, pos)
  override def read(dst : JByteBuffer, pos:Long) = wrapped.read(dst, pos)
}
