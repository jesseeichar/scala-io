package scalax.io

import scalax.io.ResourceAdapting.{ChannelInputStreamAdapter, ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter}
import java.io.{OutputStream, Reader, Writer, InputStream}
import java.nio.channels.{ReadableByteChannel, WritableByteChannel}
import StandardOpenOption._

/**
 * A ManagedResource for accessing and using SeekableByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class SeekableByteChannelResource[+A <: SeekableByteChannel] (
    opener: (Seq[OpenOption])=> A,
    closeAction:CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None,
    descName:ResourceDescName = UnknownName(),
    protected val openOptions:Option[Seq[OpenOption]] = None)
  extends SeekableResource[A]
  with ResourceOps[A, SeekableByteChannelResource[A]]  {
  
  self => 
  private def rawOpen() = opener(openOptions getOrElse ReadWrite)
  def open():OpenedResource[A] = new CloseableOpenedResource(rawOpen(),closeAction)
  def unmanaged = new SeekableByteChannelResource[A](opener, CloseAction.Noop, sizeFunc, descName, openOptions) with UnmanagedResource {
    private[this] val resource = self.open()
    override def open = new UnmanagedOpenedResource(resource.get)
    def close = resource.close()
  }

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(rawOpen(), isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new InputStreamResource(nResource,closer,sizeFunc,descName)
    else new InputStreamResource(nResource,closer,sizeFunc,descName) with UnmanagedResourceAdapter
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(rawOpen(), isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource, closer)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(rawOpen(),sourceCodec, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new ReaderResource(nResource, closer)
    else new ReaderResource(nResource, closer) with UnmanagedResourceAdapter
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(rawOpen(),sourceCodec, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new WriterResource(nResource, closer)
    else new WriterResource(nResource, closer) with UnmanagedResourceAdapter
  }
  def writableByteChannel = 
    if(isManaged) new WritableByteChannelResource(rawOpen(), closeAction)
    else new WritableByteChannelResource(rawOpen(), closeAction) with UnmanagedResourceAdapter
    
  def readableByteChannel = 
    if(isManaged) new ReadableByteChannelResource(rawOpen(),closeAction,sizeFunc,descName)
    else new ReadableByteChannelResource(rawOpen(),closeAction,sizeFunc,descName) with UnmanagedResourceAdapter
  
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

    new CloseableOpenedResource(resource,closeAction)
  }

  protected override def underlyingOutput: OutputResource[OutputStream] = outputStream
  override def blocks(blockSize: Option[Int] = None):LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)

  override def bytesAsInts:LongTraversable[Int] = ResourceTraversable.seekableByteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes:LongTraversable[Byte] = ResourceTraversable.seekableByteChannelBased[Byte,Byte](this.open, sizeFunc)

  override def toString: String = "SeekableByteChannelResource("+descName.name+")"
}

