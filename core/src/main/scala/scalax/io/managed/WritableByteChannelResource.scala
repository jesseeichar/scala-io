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
    val context:ResourceContext[A])
  extends OutputResource[A]
  with ResourceOps[A, OutputResource[A], WritableByteChannelResource[A]]  {


  self => 

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,context)
  def unmanaged = new scalax.io.unmanaged.WritableByteChannelResource[A](opener, context)

  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(opener, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new OutputStreamResource(nResource, newContext)
  }
  def underlyingOutput = outputStream
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(opener,sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WriterResource(nResource, newContext)
  }
  def writableByteChannel = this
}
