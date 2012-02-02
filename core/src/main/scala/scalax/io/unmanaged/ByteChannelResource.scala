package scalax.io
package unmanaged

import java.nio.channels.{ByteChannel, Channels}
import java.io.Reader
import scalax.io.ResourceAdapting.{ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter, ChannelInputStreamAdapter}

/**
 * A for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ByteChannelResource[+A <: ByteChannel] (
    resource: A,
    closeAction:CloseAction[A] = CloseAction.Noop)
  extends InputResource[A]
  with OutputResource[A]
  with ResourceOps[A, ByteChannelResource[A]] with UnmanagedResource {
  self => 
  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource)
  override def close() = new CloseableOpenedResource(open.get, closeAction).close()
  override def unmanaged = this
  protected def sizeFunc = () => None
  
  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,closer, UnknownName())
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource,closer)
  }
  def underlyingOutput = outputStream
  def reader(implicit sourceCodec: Codec)  = {
    def nResource = new ChannelReaderAdapter(resource, sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, closer)
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource, sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, closer)
  }

  def writableByteChannel = new WritableByteChannelResource(resource, closeAction)
  def readableByteChannel = new ReadableByteChannelResource(resource, closeAction)
  
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize, open)
  
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  }
