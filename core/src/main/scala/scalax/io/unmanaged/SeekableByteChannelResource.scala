package scalax.io
package unmanaged

import scalax.io.ResourceAdapting.{ChannelInputStreamAdapter, ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter}
import java.io.{OutputStream, Reader, Writer, InputStream}
import java.nio.channels.{ReadableByteChannel, WritableByteChannel}
import StandardOpenOption._

/**
 * A ManagedResource for accessing and using SeekableByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class SeekableByteChannelResource[+A <: SeekableByteChannel] (
    resource: A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None)
  extends SeekableResource[A]
  with ResourceOps[A, SeekableResource[A], SeekableByteChannelResource[A]]
  with UnmanagedResource {

  self => 

  override final val open:OpenedResource[A] = new UnmanagedOpenedResource(resource, unmanagedContext(context))
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override def updateContext(newContext:ResourceContext) = 
    new SeekableByteChannelResource(resource, newContext, closeAction, sizeFunc)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new SeekableByteChannelResource(resource, context, newCloseAction :+ closeAction, sizeFunc)
  override final val unmanaged = this

  override def inputStream = {
    def nResource = new ChannelInputStreamAdapter(resource)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,context, closer, sizeFunc)
  }
  override def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource, context, closer)
  }
  override def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(resource,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, context, closer)
  }
  override def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }
  override def writableByteChannel = new WritableByteChannelResource(resource, context, closeAction)
    
  override def readableByteChannel = new ReadableByteChannelResource(resource,context, closeAction, sizeFunc)
  
  protected override def underlyingChannel(append:Boolean) = {
    val r:A = append match {
      case true =>
        val c = resource
        val pos = c.position
        val size = c.size
        if(pos < c.size) {
          c.position(c.size)
        }
        val newPos = c.position
        c
      case false =>
        resource
    }

    new UnmanagedOpenedResource(r,unmanagedContext(context))
  }

  protected override def underlyingOutput: OutputResource[OutputStream] = outputStream
  override def blocks(blockSize: Option[Int] = None):LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize, context, safeSizeFunc, open)

  override def bytesAsInts:LongTraversable[Int] = ResourceTraversable.seekableByteChannelBased[Byte,Int](this.open, context, safeSizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes:LongTraversable[Byte] = ResourceTraversable.seekableByteChannelBased[Byte,Byte](this.open, context, safeSizeFunc)

  override def toString: String = "SeekableByteChannelResource("+context.descName.name+")"
}

