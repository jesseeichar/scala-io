package scalax.io
package managed

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
    opener: => A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None)
  extends InputResource[A]
  with ResourceOps[A, InputStreamResource[A]] {

  self => 

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,context, closeAction)
  
  def updateContext(newContext:ResourceContext) = new InputStreamResource(opener, newContext, closeAction, sizeFunc)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new InputStreamResource(opener, context, newCloseAction :+ closeAction, sizeFunc)

  def inputStream = this

  override def toString: String = "InputStreamResource("+context.descName.name+")"

  def reader(implicit sourceCodec: Codec) = {
    def nResource = {
      val a = open()
      new InputStreamReader(a.get, sourceCodec.charSet) with Adapter[A] {
        def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource,context, closer)
  }

  def readableByteChannel:ReadableByteChannelResource[ReadableByteChannel] = {
    def nResource = new ReadableChannelAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReadableByteChannelResource(nResource, context, closer, sizeFunc)
  }
  def chars(implicit codec: Codec) = reader(codec).chars
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = {
    def toChannelOpen = {
      val opened = open
      val closer = CloseAction((_:ReadableByteChannel) => if(false) opened.close()) // WTF see other InputStreamResource
      
      new CloseableOpenedResource (Channels.newChannel(opened.get), context, closer)
    }
    new traversable.ChannelBlockLongTraversable(blockSize, context, sizeFunc, toChannelOpen)
  }

  override def bytesAsInts : LongTraversable[Int] = readableByteChannel.bytesAsInts
  override def bytes : LongTraversable[Byte] = readableByteChannel.bytes
}
