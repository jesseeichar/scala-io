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
    val context:ResourceContext[A])
  extends InputResource[A]
  with OutputResource[A]
  with ResourceOps[A, InputResource[A] with OutputResource[A], ByteChannelResource[A]] with UnmanagedResource {

  self => 

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource,context)
  override def close() = new CloseableOpenedResource(open.get, context).close()
  override def unmanaged = this
  def newContext[R >: A](newContext:ResourceContext[R]) = new ByteChannelResource(resource, newContext)
  protected def sizeFunc = () => None
  
  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(resource, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new InputStreamResource(nResource,newContext)
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new OutputStreamResource(nResource,newContext)
  }
  def underlyingOutput = outputStream
  def reader(implicit sourceCodec: Codec)  = {
    def nResource = new ChannelReaderAdapter(resource, sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new ReaderResource(nResource, newContext)
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource, sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WriterResource(nResource, newContext)
  }

  def writableByteChannel = new WritableByteChannelResource(resource, context)
  def readableByteChannel = new ReadableByteChannelResource(resource, context)
  
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize, open)
  
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  }
