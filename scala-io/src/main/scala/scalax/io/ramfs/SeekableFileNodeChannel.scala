/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2009-2010, Jesse Eichar          **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scalax.io.ramfs

import scalax.io.{
  SeekableByteChannel, OpenOption
}
import OpenOption._
import java.io.IOException
import java.lang.Math.min


private[ramfs] class SeekableFileNodeChannel(node:FileNode, owner:RamPath, openOptions: OpenOption*) extends SeekableByteChannel {
  
  var closed = false  
  var position = 0L
  
  if(openOptions contains Truncate) node.data.clear()
  if(openOptions contains Append) position = node.data.size
  
  def s = new String(node.data.toArray).mkString(",").replaceAll("\n","\\\\n")
  
  def write (src: java.nio.ByteBuffer):Int = {
    val traversable = new Traversable[Byte]() {
      def foreach[U](f: Byte => U): Unit = {
        (src.position until src.limit) foreach {i => f(src get i)}
      }
      
      override lazy val size = super.size
    }

    node.data.remove(position.toInt, min(traversable.size, node.data.size - position.toInt))
    node.data.insertAll(position.toInt, traversable)
    position += traversable.size
    
    src.position(src.limit)
    
    traversable.size
  }
  def truncate (size: Long):scalax.io.SeekableByteChannel = {
    node.data = node.data.take(size.toInt)    
    position = min(node.data.size, size)
    this
  }
  def size = node.data.size
  def read (dst: java.nio.ByteBuffer):Int = {
    if(position.toInt < node.data.size) {
      val toRead = node.data.view.slice(position.toInt, min(position.toInt + dst.limit, node.data.size))
      toRead foreach {b => dst.put(b)}    
      position += toRead.size
      toRead.size
    } else {
      -1
    }
  }
  def position (newPosition: Long) = {
    // TODO maybe new exception?
    if(newPosition > Int.MaxValue) throw new IOException(newPosition + " is larger than a Ram file can be");
    position = newPosition.toInt
    this
  }

  def close() = {
    closed = true
    if(openOptions contains DeleteOnClose) owner.delete(force=true)
  }
  def isOpen() = closed
  
}