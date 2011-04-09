package scalax.io

import java.nio.channels.Channels
import scalax.io.ResourceAdapting.{ChannelInputStreamAdapter, ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter}

/**
 * A ManagedResource for accessing and using SeekableByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class SeekableByteChannelResource[+A <: SeekableByteChannel] (
    opener: => A,
    closeAction:CloseAction[A],
    protected val sizeFunc:() => Option[Long])
  extends SeekableResource[A]
  with ResourceOps[A, SeekableByteChannelResource[A]]  {

  def open() = opener
  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new SeekableByteChannelResource(opener,newAction :+ closeAction,sizeFunc)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new SeekableByteChannelResource(opener,closeAction +: newAction,sizeFunc)

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,closer,sizeFunc)
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromOutputStream(nResource).appendCloseAction(closer)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(opener,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromReader(nResource).appendCloseAction(closer)
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromWriter(nResource).appendCloseAction(closer)
  }
  def writableByteChannel = Resource.fromWritableByteChannel(opener).appendCloseAction(closeAction)
  def readableByteChannel = new ReadableByteChannelResource(opener,closeAction,sizeFunc)
  def byteChannel = new ByteChannelResource(opener,closeAction,sizeFunc)

  override def bytesAsInts = inputStream.bytesAsInts // TODO optimize for byteChannel
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel

  protected def channel(openOptions:OpenOption*) = this
}

