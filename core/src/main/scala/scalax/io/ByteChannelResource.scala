package scalax.io

import java.nio.channels.{ByteChannel, Channels}
import scalax.io.ResourceAdapting.{ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter, ChannelInputStreamAdapter}

/**
 * A for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ByteChannelResource[+A <: ByteChannel] (
    opener: => A,
    closeAction:CloseAction[A],
    protected val sizeFunc:() => Option[Long])
  extends InputResource[A]
  with OutputResource[A]
  with ResourceOps[A, ByteChannelResource[A]] {

  def open() = new CloseableOpenedResource(opener,closeAction)

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new ByteChannelResource(opener,newAction :+ closeAction,sizeFunc)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new ByteChannelResource(opener,closeAction +: newAction,sizeFunc)

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromInputStream(nResource).appendCloseAction(closer)
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromOutputStream(nResource).appendCloseAction(closer)
  }
  def underlyingOutput = outputStream
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
  def readableByteChannel = Resource.fromReadableByteChannel(opener).appendCloseAction(closeAction)

  def bytesAsInts = inputStream.bytesAsInts // TODO optimize for byteChannel
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
}
