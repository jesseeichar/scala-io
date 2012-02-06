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
    val context: ResourceContext[A],
    protected val openOptions:Option[Seq[OpenOption]] = None)
  extends SeekableResource[A]
  with ResourceOps[A, InputResource[A] with OutputResource[A], SeekableByteChannelResource[A]]
  with UnmanagedResource {

  self => 

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource, context)
  override def close() = new CloseableOpenedResource(open.get, context).close()
  override def unmanaged = this
  protected def sizeFunc = () => None

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(resource, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new InputStreamResource(nResource,newContext)
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new OutputStreamResource(nResource, newContext)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(resource,sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new ReaderResource(nResource, newContext)
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource,sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WriterResource(nResource, newContext)
  }
  def writableByteChannel = new WritableByteChannelResource(resource, context)
    
  def readableByteChannel = new ReadableByteChannelResource(resource,context)
  
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

    new CloseableOpenedResource(r,context)
  }

  protected override def underlyingOutput: OutputResource[OutputStream] = outputStream
  override def blocks(blockSize: Option[Int] = None):LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)

  override def bytesAsInts:LongTraversable[Int] = ResourceTraversable.seekableByteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes:LongTraversable[Byte] = ResourceTraversable.seekableByteChannelBased[Byte,Byte](this.open, sizeFunc)

  override def toString: String = "SeekableByteChannelResource("+context.descName.name+")"
}

