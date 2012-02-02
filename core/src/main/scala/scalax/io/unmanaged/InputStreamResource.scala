package scalax.io
package unmanaged

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
    resource: A,
    closeAction:CloseAction[A] = CloseAction.Noop,
    descName:ResourceDescName = UnknownName())
  extends InputResource[A]
  with ResourceOps[A, InputStreamResource[A]]
  with UnmanagedResource {
  
  self => 
  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource)
  override def close() = new CloseableOpenedResource(open.get, closeAction).close()
  override def unmanaged = this
  protected def sizeFunc = () => None

  def inputStream = this

  override def toString: String = "InputStreamResource("+descName.name+")"

  def reader(implicit sourceCodec: Codec) = {
    def nResource = {
      val a = open()
      new InputStreamReader(a.get) with Adapter[A] {
        def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource,closer,descName)
  }

  def readableByteChannel:ReadableByteChannelResource[ReadableByteChannel] = {
    def nResource = new ReadableChannelAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReadableByteChannelResource(nResource, closer, descName)
  }
  def chars(implicit codec: Codec) = reader(codec).chars
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = {
    def toChannelOpen = {
      val opened = open
      val closeAction = CloseAction((_:ReadableByteChannel) => if(false) opened.close())
      new CloseableOpenedResource (Channels.newChannel(opened.get), closeAction)
    }
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, toChannelOpen)
  }

  override def bytesAsInts : LongTraversable[Int] = readableByteChannel.bytesAsInts
  override def bytes : LongTraversable[Byte] = readableByteChannel.bytes
}
