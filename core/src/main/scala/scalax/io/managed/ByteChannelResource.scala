package scalax.io
package managed

import java.nio.channels.{ByteChannel, Channels}
import java.io.Reader
import scalax.io.ResourceAdapting.{ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter, ChannelInputStreamAdapter}

/**
 * A for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ByteChannelResource[+A <: ByteChannel] (
    opener: => A,
    val context:ResourceContext[A],
    protected val sizeFunc:() => Option[Long] = () => None)
  extends InputResource[A]
  with OutputResource[A]
  with ResourceOps[A, InputResource[A] with OutputResource[A], ByteChannelResource[A]] {

  self => 
  
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,context)

  def unmanaged = new scalax.io.unmanaged.ByteChannelResource[A](opener, context)
  def newContext[R >: A](newContext:ResourceContext[R]) = new ByteChannelResource(opener, newContext, sizeFunc)
  
  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(opener, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new InputStreamResource(nResource,newContext, sizeFunc)
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new OutputStreamResource(nResource,newContext)
  }
  def underlyingOutput = outputStream
  def reader(implicit sourceCodec: Codec)  = {
    def nResource = new ChannelReaderAdapter(opener, sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new ReaderResource(nResource, newContext)
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener, sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WriterResource(nResource, newContext)
  }

  def writableByteChannel = new WritableByteChannelResource(opener, context)
  def readableByteChannel = new ReadableByteChannelResource(opener, context, sizeFunc)
  
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)
  
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  }
