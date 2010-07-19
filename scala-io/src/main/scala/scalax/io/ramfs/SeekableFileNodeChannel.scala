/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import scalax.io.SeekableByteChannel
import java.io.IOException
import java.lang.Math.min


private[ramfs] class SeekableFileNodeChannel(node:FileNode) extends SeekableByteChannel {
  var position = 0L
  def write (src: java.nio.ByteBuffer):Int = {
    
    val traversable = new Traversable[Byte]() {
      def foreach[U](f: Byte => U): Unit = {
        (src.position to src.limit) foreach {i => f(src get i)}
      }
      
      override lazy val size = super.size
    }

    node.data.remove(position.toInt, min(traversable.size, node.data.size))
    node.data.insertAll(position.toInt, traversable)
    
    position += traversable.size
    
    src.position(src.limit)
    
    traversable.size
  }
  def truncate (size: Long):scalax.io.SeekableByteChannel = {
    node.data = node.data.take(size.toInt)
    this
  }
  def size = node.data.size
  def read (dst: java.nio.ByteBuffer):Int = {
    val toRead = node.data.view.slice(position.toInt, min(dst.limit, node.data.size))
    
    toRead foreach {b => dst.put(b)}
    
    toRead.size
  }
  def position (newPosition: Long) = {
    // TODO maybe new exception?
    if(newPosition > Int.MaxValue) throw new IOException(newPosition + " is larger than a Ram file can be");
    position = newPosition.toInt
    this
  }

  def close() = ()// No need for an operation
  def isOpen() = true // always open (can't be closed because it is unecessary)
  
}