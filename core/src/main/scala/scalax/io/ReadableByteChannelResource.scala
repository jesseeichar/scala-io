package scalax.io

import java.io.BufferedInputStream
import java.nio.channels.{Channels, ReadableByteChannel}

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the {{scalax.io.Resource}} object.
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel] protected[io](opener: => A, closeAction:CloseAction[A]) extends BufferableInputResource[A, BufferedInputStream]
    with ResourceOps[A, ReadableByteChannelResource[A]] {
  def open() = opener
  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new ReadableByteChannelResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new ReadableByteChannelResource(opener,closeAction +: newAction)

  def buffered = inputStream.buffered
  def inputStream = {
    val nResource = new ChannelInputStreamAdapter(opener)
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromInputStream(nResource)(closer)
  }
  def reader(implicit sourceCodec: Codec) = {
    val nResource = new ChannelReaderAdapter(opener,sourceCodec)
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromReader(nResource)(closer)
  }
  def readableByteChannel = this
  def bytesAsInts = inputStream.bytesAsInts // TODO optimize for byteChannel
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
}
