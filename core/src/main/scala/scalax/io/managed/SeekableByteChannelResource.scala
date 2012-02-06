package scalax.io
package managed

import scalax.io.ResourceAdapting.{ChannelInputStreamAdapter, ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter}
import java.io.{OutputStream, Reader, Writer, InputStream}
import java.nio.channels.{ReadableByteChannel, WritableByteChannel}
import StandardOpenOption._

/**
 * A ManagedResource for accessing and using SeekableByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class SeekableByteChannelResource[+A <: SeekableByteChannel] (
    opener: (Seq[OpenOption])=> A,
        val context:ResourceContext[A],
    protected val sizeFunc:() => Option[Long] = () => None,
    protected val openOptions:Option[Seq[OpenOption]] = None)
  extends SeekableResource[A]
  with ResourceOps[A, InputResource[A] with OutputResource[A], SeekableByteChannelResource[A]]  {
  

  self => 

  private def rawOpen() = opener(openOptions getOrElse ReadWrite)
  def open():OpenedResource[A] = new CloseableOpenedResource(rawOpen(),context)
  def unmanaged = new scalax.io.unmanaged.SeekableByteChannelResource[A](rawOpen(), context, openOptions)

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(rawOpen(), false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new InputStreamResource(nResource,newContext,sizeFunc)
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(rawOpen(), false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new OutputStreamResource(nResource, newContext)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(rawOpen(),sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new ReaderResource(nResource, newContext)
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(rawOpen(),sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WriterResource(nResource, newContext)
  }
  def writableByteChannel = new WritableByteChannelResource(rawOpen(), context)
    
  def readableByteChannel = new ReadableByteChannelResource(rawOpen(),context,sizeFunc)
  
  protected override def underlyingChannel(append:Boolean) = {
    val resource:A = append match {
      case true =>
        val c = opener(ReadWrite :+ Append)
        val pos = c.position
        val size = c.size
        if(pos < c.size) {
          c.position(c.size)
        }
        val newPos = c.position
        c
      case false =>
        opener(ReadWrite)
    }

    new CloseableOpenedResource(resource,context)
  }

  protected override def underlyingOutput: OutputResource[OutputStream] = outputStream
  override def blocks(blockSize: Option[Int] = None):LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)

  override def bytesAsInts:LongTraversable[Int] = ResourceTraversable.seekableByteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes:LongTraversable[Byte] = ResourceTraversable.seekableByteChannelBased[Byte,Byte](this.open, sizeFunc)

  override def toString: String = "SeekableByteChannelResource("+context.descName.name+")"
}

