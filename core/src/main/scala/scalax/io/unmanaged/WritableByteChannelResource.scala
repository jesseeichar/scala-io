package scalax.io
package unmanaged

import java.nio.channels.WritableByteChannel
import scalax.io.ResourceAdapting.ChannelOutputStreamAdapter

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WritableByteChannelResource[+A <: WritableByteChannel] (
    resource: A,
    resourceContext:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends Output {

  self =>
  final val context = unmanagedContext(resourceContext)
  //def open:OpenedResource[A] = new UnmanagedOpenedResource(resource, unmanagedContext(context))
  protected override val underlyingOutput = {
    val nResource = new ChannelOutputStreamAdapter(resource)
    val closer = ResourceAdapting.closeAction(CloseAction.Noop)
    new managed.OutputStreamResource(nResource, context, closer) {
      override def open = new UnmanagedOpenedResource[ChannelOutputStreamAdapter[A]](nResource, context)
    }
  }
}
