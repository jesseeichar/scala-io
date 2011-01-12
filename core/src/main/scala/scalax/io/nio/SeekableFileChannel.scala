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


class SeekableFileChannel(val self : FileChannel) extends SeekableByteChannel with Proxy {
  def write (src: java.nio.ByteBuffer) = self.write(src)
  def truncate (size: Long)  = new SeekableFileChannel(self.truncate(size))
  def size = self.size
  def read (dst: java.nio.ByteBuffer) = self.read(dst)
  def position (newPosition: Long) = new SeekableFileChannel(self.position(newPosition))
  def position = self.position
  def close = self.close
  def isOpen = self.isOpen

  override def write(src : JByteBuffer, pos:Long) = self.write(src, pos)
  override def read(dst : JByteBuffer, pos:Long) = self.read(dst, pos)
}
