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
    closeAction:CloseAction[A] = CloseAction.Noop,
    descName:ResourceDescName = UnknownName())
  extends InputResource[A]
  with ResourceOps[A, ReadableByteChannelResource[A]]
  with UnmanagedResource {
  self => 
  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource)
  override def close() = new CloseableOpenedResource(open.get, closeAction).close()
  override def unmanaged = this
  protected def sizeFunc = () => None
  
  def inputStream:InputResource[InputStream] = {
    def nResource = new ChannelInputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource, closer, descName)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(resource,sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, closer)
  }
  def readableByteChannel:InputResource[ReadableByteChannel] = this
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)

  override def toString: String = "ReadableByteChannelResource ("+descName.name+")"
}
