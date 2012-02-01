package scalax.io

import java.io.BufferedInputStream
import java.nio.channels.{Channels, ReadableByteChannel}
import scalax.io.ResourceAdapting.{ChannelReaderAdapter, ChannelInputStreamAdapter}
import java.io.Reader
import java.io.InputStream

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel] (
    opener: => A,
    closeAction:CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None,
    descName:ResourceDescName = UnknownName())
  extends InputResource[A]
  with ResourceOps[A, ReadableByteChannelResource[A]] {
  self => 
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new ReadableByteChannelResource[A](opener, CloseAction.Noop, sizeFunc, descName) with UnmanagedResource {
    private[this] val resource = self.open
    override def open = new UnmanagedOpenedResource(resource.get)
    def close() = resource.close()
  }

  def inputStream:InputResource[InputStream] = {
    def nResource = new ChannelInputStreamAdapter(opener, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new InputStreamResource(nResource, closer, sizeFunc,descName)
    else new InputStreamResource(nResource, closer, sizeFunc,descName) with UnmanagedResourceAdapter
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(opener,sourceCodec, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new ReaderResource(nResource, closer)
    else new ReaderResource(nResource, closer) with UnmanagedResourceAdapter
  }
  def readableByteChannel:InputResource[ReadableByteChannel] = this
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)

  override def toString: String = "ReadableByteChannelResource ("+descName.name+")"
}
