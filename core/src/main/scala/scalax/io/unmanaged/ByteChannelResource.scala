package scalax.io
package unmanaged

import java.nio.channels.{ByteChannel, Channels}
import java.io.Reader
import scalax.io.ResourceAdapting.{ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter, ChannelInputStreamAdapter}
import java.io.InputStream
import java.io.OutputStream
import java.io.Writer

/**
 * A for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ByteChannelResource[+A <: ByteChannel] (
    resource: A,
    val context:ResourceContext = ResourceContext(),
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends InputResource[A]
  with OutputResource[A]
  with ResourceOps[A, InputResource[A] with OutputResource[A], ByteChannelResource[A]] with UnmanagedResource {

  self => 

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource,context)
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override def unmanaged = this
  override def newContext(newContext:ResourceContext) = 
    new ByteChannelResource(resource, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new ByteChannelResource(resource, context, newCloseAction :+ closeAction)
  protected def sizeFunc = () => None
  
  override def inputStream = {
    def nResource = new ChannelInputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,context, closer)
  }
  override def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource,context, closer)
  }
  protected override def underlyingOutput = outputStream
  override def reader(implicit sourceCodec: Codec)  = {
    def nResource = new ChannelReaderAdapter(resource, sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, context, closer)
  }
  override def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource, sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }

  override def writableByteChannel = new WritableByteChannelResource(resource, context, closeAction)
  override def readableByteChannel = new ReadableByteChannelResource(resource, context, closeAction)
  
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize, open)
  
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  }
