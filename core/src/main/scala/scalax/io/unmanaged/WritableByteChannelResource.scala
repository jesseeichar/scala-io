package scalax.io
package unmanaged

import java.io.BufferedOutputStream
import java.nio.channels.{Channels, WritableByteChannel}
import scalax.io.ResourceAdapting.{ChannelOutputStreamAdapter, ChannelWriterAdapter}
import java.io.OutputStream
import java.io.Writer

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WritableByteChannelResource[+A <: WritableByteChannel] (
    resource: A,
    val context:ResourceContext = ResourceContext(),
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends OutputResource[A]
  with ResourceOps[A, OutputResource[A], WritableByteChannelResource[A]]
  with UnmanagedResource {

  self => 

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource, context)
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override def newContext(newContext:ResourceContext) = 
    new WritableByteChannelResource(resource, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new WritableByteChannelResource(resource, context, newCloseAction :+ closeAction)
  override def unmanaged = this

  override def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(resource, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource, context, closer)
  }
  protected override def underlyingOutput = outputStream
  override def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(resource,sourceCodec, false)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }
  override def writableByteChannel = this
}
