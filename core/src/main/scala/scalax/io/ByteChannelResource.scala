package scalax.io

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
  with ResourceOps[A, ByteChannelResource[A]] {
  self => 
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new ByteChannelResource[A](opener, CloseAction.Noop, sizeFunc) with UnmanagedResource {
    private[this] val resource = open
    override def open = new UnmanagedOpenedResource(resource.get)
    def close() = resource.close()
  }

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(opener, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new InputStreamResource(nResource,closer, sizeFunc, UnknownName())
    else new InputStreamResource(nResource,closer, sizeFunc, UnknownName()) with UnmanagedResourceAdapter
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new OutputStreamResource(nResource,closer)
    else new OutputStreamResource(nResource,closer) with UnmanagedResourceAdapter
  }
  def underlyingOutput = outputStream
  def reader(implicit sourceCodec: Codec)  = {
    def nResource = new ChannelReaderAdapter(opener, sourceCodec, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if (isManaged) new ReaderResource(nResource, closer)
    else new ReaderResource(nResource, closer) with UnmanagedResourceAdapter
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener, sourceCodec, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if (isManaged) new WriterResource(nResource, closer)
    else new WriterResource(nResource, closer) with UnmanagedResourceAdapter
  }
  def writableByteChannel =
    if (isManaged) new WritableByteChannelResource(opener, closeAction)
    else new WritableByteChannelResource(opener, closeAction) with UnmanagedResourceAdapter

  def readableByteChannel =
    if (isManaged) new ReadableByteChannelResource(opener, closeAction, sizeFunc)
    else new ReadableByteChannelResource(opener, closeAction, sizeFunc) with UnmanagedResourceAdapter

  
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)
  
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  }
