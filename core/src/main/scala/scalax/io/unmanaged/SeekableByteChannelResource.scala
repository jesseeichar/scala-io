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
    val context:ResourceContext = ResourceContext(),
    closeAction: CloseAction[A] = CloseAction.Noop,
    protected val openOptions:Option[Seq[OpenOption]] = None)
  extends SeekableResource[A]
  with ResourceOps[A, SeekableResource[A], SeekableByteChannelResource[A]]
  with UnmanagedResource {

  self => 

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource, context)
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override def newContext(newContext:ResourceContext) = 
    new SeekableByteChannelResource(resource, newContext, closeAction, openOptions)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new SeekableByteChannelResource(resource, context, newCloseAction :+ closeAction, openOptions)
  override def unmanaged = this
  protected override def sizeFunc = () => None

  override def inputStream = {
    def nResource = new ChannelInputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,context, closer)
  }
  override def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource, context, closer)
  }
  override def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(resource,sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, context, closer)
  }
  override def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource,sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }
  override def writableByteChannel = new WritableByteChannelResource(resource, context, closeAction)
    
  override def readableByteChannel = new ReadableByteChannelResource(resource,context, closeAction)
  
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

    new CloseableOpenedResource(r,context, closeAction)
  }

  protected override def underlyingOutput: OutputResource[OutputStream] = outputStream
  override def blocks(blockSize: Option[Int] = None):LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)

  override def bytesAsInts:LongTraversable[Int] = ResourceTraversable.seekableByteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes:LongTraversable[Byte] = ResourceTraversable.seekableByteChannelBased[Byte,Byte](this.open, sizeFunc)

  override def toString: String = "SeekableByteChannelResource("+context.descName.name+")"
}

