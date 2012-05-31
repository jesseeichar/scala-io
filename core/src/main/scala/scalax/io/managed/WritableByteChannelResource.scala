package scalax.io
package managed

import java.io.BufferedOutputStream
import java.nio.channels.{Channels, WritableByteChannel}
import scalax.io.ResourceAdapting.{ChannelOutputStreamAdapter, ChannelWriterAdapter}
import java.io.OutputStream
import java.io.Writer

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WritableByteChannelResource[+A <: WritableByteChannel] (
    opener: => A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends OutputResource[A]
  with ResourceOps[A, WritableByteChannelResource[A]]  {


  self =>

  override def open():OpenedResource[A] = new CloseableOpenedResource(opener,context, closeAction)
  override def updateContext(newContext:ResourceContext) = new WritableByteChannelResource(opener, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) =
    new WritableByteChannelResource(opener, context, newCloseAction :+ closeAction)

  override def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource, context, closer)
  }
  protected override def underlyingOutput = this
  override def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }
  override def writableByteChannel = this
}
