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
    closeAction:CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None)
  extends InputResource[A]
  with OutputResource[A]
  with ResourceOps[A, InputResource[A] with OutputResource[A]] {
  self => 
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)

  def unmanaged = new scalax.io.unmanaged.ByteChannelResource[A](opener, closeAction)

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(opener, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,closer, sizeFunc, UnknownName())
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource,closer)
  }
  def underlyingOutput = outputStream
  def reader(implicit sourceCodec: Codec)  = {
    def nResource = new ChannelReaderAdapter(opener, sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, closer)
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener, sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, closer)
  }

  def writableByteChannel = new WritableByteChannelResource(opener, closeAction)
  def readableByteChannel = new ReadableByteChannelResource(opener, closeAction, sizeFunc)
  
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)
  
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  }
