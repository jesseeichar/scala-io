package scalax.io
package managed

import java.io.BufferedInputStream
import java.nio.channels.{Channels, ReadableByteChannel}
import scalax.io.ResourceAdapting.{ChannelReaderAdapter, ChannelInputStreamAdapter}
import java.io.Reader
import java.io.InputStream

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel] (
    opener: => A,
    val context:ResourceContext[A],
    protected val sizeFunc:() => Option[Long] = () => None)
  extends InputResource[A]
  with ResourceOps[A, InputResource[A], ReadableByteChannelResource[A]] {

  self => 

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,context)
  def unmanaged = new scalax.io.unmanaged.ReadableByteChannelResource[A](opener, context)

  def inputStream:InputResource[InputStream] = {
    def nResource = new ChannelInputStreamAdapter(opener, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new InputStreamResource(nResource, newContext, sizeFunc)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(opener,sourceCodec, false)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new ReaderResource(nResource, newContext)
  }
  def readableByteChannel:InputResource[ReadableByteChannel] = this
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = 
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, open)

  override def toString: String = "ReadableByteChannelResource ("+context.descName.name+")"
}
