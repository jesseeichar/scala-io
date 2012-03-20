package scalax.io
package unmanaged

import java.nio.channels.WritableByteChannel
import java.nio.ByteBuffer

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
    val uncloseableChannel = new WritableByteChannel{
      def isOpen = resource.isOpen
      def write(src: ByteBuffer) = resource.write(src)
      override def close() {}
    }
    new managed.WritableByteChannelResource[WritableByteChannel](uncloseableChannel, resourceContext, CloseAction.Noop)
  }
}
