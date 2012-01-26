package scalax.io

import java.io.BufferedOutputStream
import java.nio.channels.{Channels, WritableByteChannel}
import scalax.io.ResourceAdapting.{ChannelOutputStreamAdapter, ChannelWriterAdapter}

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WritableByteChannelResource[+A <: WritableByteChannel] (
    opener: => A,
    closeAction:CloseAction[A])
  extends OutputResource[A]
  with ResourceOps[A, WritableByteChannelResource[A]]  {

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new WritableByteChannelResource[A](opener, closeAction) {
    private[this] val resource = opener
    override def open = new UnmanagedOpenedResource(resource, closeAction)
  }

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new WritableByteChannelResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new WritableByteChannelResource(opener,closeAction +: newAction)

  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromOutputStream(nResource).appendCloseAction(closer)
  }
  def underlyingOutput = outputStream
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromWriter(nResource).appendCloseAction(closer)
  }
  def writableByteChannel = this
}
