package scalax.io

import java.io.BufferedInputStream
import java.nio.channels.{Channels, ReadableByteChannel}
import scalax.io.ResourceAdapting.{ChannelReaderAdapter, ChannelInputStreamAdapter}

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel] (
    opener: => A,
    closeAction:CloseAction[A],
    protected val sizeFunc:() => Option[Long])
  extends InputResource[A]
  with ResourceOps[A, ReadableByteChannelResource[A]] {

  def open() = new CloseableOpenedResource(opener,closeAction)

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new ReadableByteChannelResource(opener,newAction :+ closeAction,sizeFunc)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new ReadableByteChannelResource(opener,closeAction +: newAction,sizeFunc)

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource, closer, sizeFunc)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(opener,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromReader(nResource).appendCloseAction(closer)
  }
  def readableByteChannel = this
  def bytesAsInts = inputStream.bytesAsInts // TODO optimize for byteChannel
  def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
}
