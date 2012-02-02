package scalax.io
package unmanaged

import java.io.BufferedOutputStream
import java.nio.channels.{Channels, WritableByteChannel}
import scalax.io.ResourceAdapting.{ChannelOutputStreamAdapter, ChannelWriterAdapter}

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WritableByteChannelResource[+A <: WritableByteChannel] (
    resource: A,
    closeAction:CloseAction[A] = CloseAction.Noop)
  extends OutputResource[A]
  with ResourceOps[A, WritableByteChannelResource[A]]
  with UnmanagedResource {

  self =>
  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource)
  override def close() = new CloseableOpenedResource(open.get, closeAction).close()
  override def unmanaged = this

  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource, closer)
  }
  def underlyingOutput = outputStream
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource,sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, closer)
  }
  def writableByteChannel = this
}
