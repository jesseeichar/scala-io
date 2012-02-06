package scalax.io
package unmanaged

import java.io.BufferedInputStream
import java.nio.channels.{Channels, ReadableByteChannel}
import scalax.io.ResourceAdapting.{ChannelReaderAdapter, ChannelInputStreamAdapter}
import java.io.Reader
import java.io.InputStream

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel] (
    resource: A,
    val context: ResourceContext[A])
  extends InputResource[A]
  with ResourceOps[A, InputResource[A], ReadableByteChannelResource[A]]
  with UnmanagedResource {

  self => 

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource, context)
  override def close() = new CloseableOpenedResource(open.get, context).close()
  override def unmanaged = this
  protected def sizeFunc = () => None
  
  def inputStream:InputResource[InputStream] = {
    def nResource = new ChannelInputStreamAdapter(resource, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new InputStreamResource(nResource, newContext)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(resource,sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new ReaderResource(nResource, newContext)
  }
  def readableByteChannel:InputResource[ReadableByteChannel] = this
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)

  override def toString: String = "ReadableByteChannelResource ("+context.descName.name+")"
}
