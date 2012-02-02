package scalax.io
package managed

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
  with ResourceOps[A, OutputResource[A]]  {

  self =>
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new scalax.io.unmanaged.WritableByteChannelResource[A](opener, CloseAction.Noop)

  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource, closer)
  }
  def underlyingOutput = outputStream
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener,sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, closer)
  }
  def writableByteChannel = this
}
