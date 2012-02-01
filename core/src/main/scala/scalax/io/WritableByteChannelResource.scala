package scalax.io

import java.io.BufferedOutputStream
import java.nio.channels.{Channels, WritableByteChannel}
import scalax.io.ResourceAdapting.{ChannelOutputStreamAdapter, ChannelWriterAdapter}

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WritableByteChannelResource[+A <: WritableByteChannel] (
    opener: => A,
    closeAction:CloseAction[A] = CloseAction.Noop)
  extends OutputResource[A]
  with ResourceOps[A, WritableByteChannelResource[A]]  {

  self =>
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new WritableByteChannelResource[A](opener, CloseAction.Noop)  with UnmanagedResource {
    private[this] val resource = self.open
    override def open = new UnmanagedOpenedResource(resource.get)
    def close() = resource.close()
  }

  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new OutputStreamResource(nResource, closer)
    else new OutputStreamResource(nResource, closer) with UnmanagedResourceAdapter
  }
  def underlyingOutput = outputStream
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener,sourceCodec, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new WriterResource(nResource, closer)
    else new WriterResource(nResource, closer) with UnmanagedResourceAdapter
  }
  def writableByteChannel = this
}
