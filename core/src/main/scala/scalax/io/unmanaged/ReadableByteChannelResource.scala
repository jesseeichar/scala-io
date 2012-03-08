package scalax.io
package unmanaged

import java.nio.channels.{Channels, ReadableByteChannel}
import scalax.io.ResourceAdapting.{ChannelReaderAdapter, ChannelInputStreamAdapter}
import java.io.{InputStreamReader, BufferedInputStream, Reader, InputStream}

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel] (
    resource: A, 
    resourceContext:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends Input {

  final val context = unmanagedContext(resourceContext)
  final val open:OpenedResource[A] = new UnmanagedOpenedResource(resource, context)
  val sizeFunc = () => None

  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, context, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, context, sizeFunc)
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] =
    new traversable.ChannelBlockLongTraversable(blockSize, context, sizeFunc, open)
  override def chars(implicit codec: Codec = Codec.default): LongTraversable[Char] = {
    val opened = new UnmanagedOpenedResource(new InputStreamReader(Channels.newInputStream(resource), codec.name), context)
    ResourceTraversable.readerBased(opened, context)
  }
  override def size = None
}
