package scalax.io
package managed

import java.nio.channels.{ByteChannel, Channels}
import java.io.{Reader, Writer, InputStream, OutputStream}
import scalax.io.ResourceAdapting.{ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter, ChannelInputStreamAdapter}

/**
 * A for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ByteChannelResource[+A <: ByteChannel] (
    opener: => A,
    val context:ResourceContext = ResourceContext(),
    closeAction: CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None)
  extends InputResource[A]
  with OutputResource[A]
  with ResourceOps[A, InputResource[A] with OutputResource[A], ByteChannelResource[A]] {

  self => 
  
  override def open():OpenedResource[A] = new CloseableOpenedResource(opener,context, closeAction)

  override def unmanaged = new scalax.io.unmanaged.ByteChannelResource[A](opener, context, closeAction)
  override def newContext(newContext:ResourceContext) = new ByteChannelResource(opener, newContext, closeAction, sizeFunc)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new ByteChannelResource(opener, context, newCloseAction :+ closeAction, sizeFunc)
  
  override def inputStream = {
    def nResource = new ChannelInputStreamAdapter(opener, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,context, closer, sizeFunc)
  }
  override def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource,context, closer)
  }
  protected override def underlyingOutput = outputStream
  override def reader(implicit sourceCodec: Codec)  = {
    def nResource = new ChannelReaderAdapter(opener, sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, context, closer)
  }
  override def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener, sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }

  override def writableByteChannel = new WritableByteChannelResource(opener, context, closeAction)
  override def readableByteChannel = new ReadableByteChannelResource(opener, context, closeAction, sizeFunc)
  
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)
  
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  }
