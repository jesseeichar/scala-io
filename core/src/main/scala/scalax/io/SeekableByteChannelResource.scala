package scalax.io

import java.nio.channels.Channels

/**
 * A ManagedResource for accessing and using SeekableByteChannels.
 *
 * @see ManagedResource
 */
class SeekableByteChannelResource[+A <: SeekableByteChannel](opener: => A, closeAction:CloseAction[A]) extends SeekableResource[A]
    with ResourceOps[A, SeekableByteChannelResource[A]]  {
  def open() = opener
  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new SeekableByteChannelResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new SeekableByteChannelResource(opener,closeAction +: newAction)

  def inputStream = {
    val nResource = new ChannelInputStreamAdapter(opener)
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromInputStream(nResource)(closer)
  }
  def outputStream = {
    val nResource = new ChannelOutputStreamAdapter(opener)
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromOutputStream(nResource)(closer)
  }
  def reader(implicit sourceCodec: Codec) = {
    val nResource = new ChannelReaderAdapter(opener,sourceCodec)
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromReader(nResource)(closer)
  }
  def writer(implicit sourceCodec: Codec) = {
    val nResource = new ChannelWriterAdapter(opener,sourceCodec)
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromWriter(nResource)(closer)
  }
  def writableByteChannel = Resource.fromWritableByteChannel(opener)(closeAction)
  def readableByteChannel = Resource.fromReadableByteChannel(opener)(closeAction)
  def byteChannel = Resource.fromByteChannel(opener)(closeAction)

  protected def channel(openOptions:OpenOption*) = this
}

