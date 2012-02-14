package scalax.io
package unmanaged

import java.io.{InputStreamReader, Reader, BufferedInputStream, InputStream}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.ReadableChannelAdapter
import java.lang.String
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

/**
 * A ManagedResource for accessing and using InputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class InputStreamResource[+A <: InputStream] (
    resource: A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None)
  extends InputResource[A]
  with ResourceOps[A, InputResource[A], InputStreamResource[A]]
  with UnmanagedResource {

  self => 

  override final val open:OpenedResource[A] = new UnmanagedOpenedResource(resource, unmanagedContext(context))
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override final val unmanaged = this
  override def updateContext(newContext:ResourceContext) = 
    new InputStreamResource(resource, newContext, closeAction, sizeFunc)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new InputStreamResource(resource, context, newCloseAction :+ closeAction, sizeFunc)

  override def inputStream = this

  override def toString: String = "InputStreamResource("+context.descName.name+")"

  override def reader(implicit sourceCodec: Codec) = {
    def nResource = {
      val a = open
      new InputStreamReader(a.get) with Adapter[A] {
        override def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource,context, closer)
  }

  override def readableByteChannel:ReadableByteChannelResource[ReadableByteChannel] = {
    def nResource = new ReadableChannelAdapter(resource)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReadableByteChannelResource(nResource, context, closer, sizeFunc)
  }
  override def chars(implicit codec: Codec) = reader(codec).chars
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = {
    def toChannelOpen = {
      val opened = open
      val closer = CloseAction((_:ReadableByteChannel) => if(false) opened.close()) // WTF
      new CloseableOpenedResource (Channels.newChannel(opened.get), context, closer)
    }
    new traversable.ChannelBlockLongTraversable(blockSize, context, safeSizeFunc, toChannelOpen)
  }

  override def bytesAsInts : LongTraversable[Int] = readableByteChannel.bytesAsInts
  override def bytes : LongTraversable[Byte] = readableByteChannel.bytes
}
