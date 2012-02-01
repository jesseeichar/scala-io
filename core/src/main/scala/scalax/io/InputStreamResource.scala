package scalax.io

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
    closeAction:CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None,
    descName:ResourceDescName = UnknownName())
  extends InputResource[A]
  with ResourceOps[A, InputStreamResource[A]] {
  
  self => 
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = {
    val resource = self.open
    new InputStreamResource[A](resource.get, CloseAction.Noop, sizeFunc, descName) with UnmanagedResource{
        override def open = new UnmanagedOpenedResource(resource.get)
        def close() = resource.close()
    }
  }

  def inputStream = this

  override def toString: String = "InputStreamResource("+descName.name+")"

  def reader(implicit sourceCodec: Codec) = {
    def nResource = {
      val a = open()
      new InputStreamReader(a.get) with Adapter[A] {
        def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new ReaderResource(nResource,closer,descName)
    else new ReaderResource(nResource,closer,descName) with UnmanagedResourceAdapter

  }

  def readableByteChannel:ReadableByteChannelResource[ReadableByteChannel] = {
    def nResource = new ReadableChannelAdapter(opener, isManaged)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new ReadableByteChannelResource(nResource, closer, sizeFunc,descName)
    else new ReadableByteChannelResource(nResource, closer, sizeFunc,descName) with UnmanagedResourceAdapter
  }
  def chars(implicit codec: Codec) = reader(codec).chars
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = {
    def toChannelOpen = {
      val opened = open
      val closeAction = CloseAction((_:ReadableByteChannel) => if(isManaged) opened.close())
      new CloseableOpenedResource (Channels.newChannel(opened.get), closeAction)
    }
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, toChannelOpen)
  }

  override def bytesAsInts : LongTraversable[Int] = readableByteChannel.bytesAsInts
  override def bytes : LongTraversable[Byte] = readableByteChannel.bytes
}
