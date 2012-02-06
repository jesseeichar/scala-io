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
    val context: ResourceContext[A])
  extends OutputResource[A]
  with ResourceOps[A, OutputResource[A], WritableByteChannelResource[A]]
  with UnmanagedResource {

  self => 

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource, context)
  override def close() = new CloseableOpenedResource(open.get, context).close()
  override def unmanaged = this

  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new OutputStreamResource(nResource, newContext)
  }
  def underlyingOutput = outputStream
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource,sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WriterResource(nResource, newContext)
  }
  def writableByteChannel = this
}
