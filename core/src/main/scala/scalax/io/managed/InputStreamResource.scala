package scalax.io
package managed

import java.io.{InputStreamReader, Reader, BufferedInputStream, InputStream}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.ReadableChannelAdapter
import java.lang.String
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

/**
 * A ManagedResource for accessing and using InputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class InputStreamResource[+A <: InputStream] (
    opener: => A,
    val context:ResourceContext[A],
    protected val sizeFunc:() => Option[Long] = () => None)
  extends InputResource[A]
  with ResourceOps[A, InputResource[A], InputStreamResource[A]] {

  self => 

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,context)
  
  def unmanaged = new scalax.io.unmanaged.InputStreamResource[A](opener, context)
  def newContext[R >: A](newContext:ResourceContext[R]) = new InputStreamResource(opener, newContext, sizeFunc)

  def inputStream = this

  override def toString: String = "InputStreamResource("+context.descName.name+")"

  def reader(implicit sourceCodec: Codec) = {
    def nResource = {
      val a = open()
      new InputStreamReader(a.get) with Adapter[A] {
        def src = a.get
      }
    }
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new ReaderResource(nResource,newContext)
  }

  def readableByteChannel:ReadableByteChannelResource[ReadableByteChannel] = {
    def nResource = new ReadableChannelAdapter(opener, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new ReadableByteChannelResource(nResource, newContext, sizeFunc)
  }
  def chars(implicit codec: Codec) = reader(codec).chars
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = {
    def toChannelOpen = {
      val opened = open
      val closeAction = CloseAction((_:ReadableByteChannel) => if(false) opened.close()) // WTF see other InputStreamResource
      val newContext = context.copy(closeAction = closeAction)
      new CloseableOpenedResource (Channels.newChannel(opened.get), newContext)
    }
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, toChannelOpen)
  }

  override def bytesAsInts : LongTraversable[Int] = readableByteChannel.bytesAsInts
  override def bytes : LongTraversable[Byte] = readableByteChannel.bytes
}
