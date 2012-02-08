package scalax.io
package unmanaged

import java.io.BufferedInputStream
import java.nio.channels.{Channels, ReadableByteChannel}
import scalax.io.ResourceAdapting.{ChannelReaderAdapter, ChannelInputStreamAdapter}
import java.io.Reader
import java.io.InputStream

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel] (
    resource: A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None)
  extends InputResource[A]
  with ResourceOps[A, InputResource[A], ReadableByteChannelResource[A]]
  with UnmanagedResource {

  self => 

  override final val open:OpenedResource[A] = new UnmanagedOpenedResource(resource, unmanagedContext(context))
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override def updateContext(newContext:ResourceContext) = 
    new ReadableByteChannelResource(resource, newContext, closeAction, sizeFunc)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new ReadableByteChannelResource(resource, context, newCloseAction :+ closeAction, sizeFunc)
  override final val unmanaged = this
  
  override def inputStream:InputResource[InputStream] = {
    def nResource = new ChannelInputStreamAdapter(resource)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource, context, closer, sizeFunc)
  }
  override def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(resource,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, context, closer)
  }
  override def readableByteChannel:InputResource[ReadableByteChannel] = this
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize, sizeFunc, open)

  override def toString: String = "ReadableByteChannelResource ("+context.descName.name+")"
}
