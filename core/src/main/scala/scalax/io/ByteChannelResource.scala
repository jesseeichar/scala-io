package scalax.io

import java.nio.channels.{ByteChannel, Channels}

/**
 * A ManagedResource for accessing and using ByteChannels.
 *
 * @see ManagedResource
 */
class ByteChannelResource[+A <: ByteChannel](opener: => A, closeAction:CloseAction[A]) extends InputResource[A] with OutputResource[A]
    with ResourceOps[A, ByteChannelResource[A]] {
  def open() = opener
  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new ByteChannelResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new ByteChannelResource(opener,closeAction +: newAction)

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
  def underlyingOutput = outputStream
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

  def bytesAsInts = inputStream.bytesAsInts // TODO optimize for byteChannel
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
}
