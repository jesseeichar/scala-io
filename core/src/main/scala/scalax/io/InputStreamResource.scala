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
    closeAction:CloseAction[A],
    protected val sizeFunc:() => Option[Long],
    descName:ResourceDescName)
  extends InputResource[A]
  with ResourceOps[A, InputStreamResource[A]] {

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new InputStreamResource[A](opener,newAction :+ closeAction,sizeFunc,descName)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new InputStreamResource[A](opener,closeAction +: newAction,sizeFunc,descName)

  def inputStream = this

  override def toString: String = "InputStreamResource("+descName.name+")"

  def reader(implicit sourceCodec: Codec): ReaderResource[Reader] = {
    def nResource = {
      val a = open()
      new InputStreamReader(a.get) with Adapter[A] {
        def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource,closer,descName)

  }

  lazy val readableByteChannel = {
    def nResource = new ReadableChannelAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReadableByteChannelResource(nResource, closer, sizeFunc,descName)
  }
  def chars(implicit codec: Codec) = reader(codec).chars
  override def blocks(blockSize: Option[Int] = None): LongTraversable[ByteBlock] = {
    def toChannelOpen = {
      val opened = open
      new CloseableOpenedResource (Channels.newChannel(opened.get), CloseAction((_:ReadableByteChannel) => opened.close()))
    }
    new traversable.ChannelBlockLongTraversable(blockSize orElse sizeFunc().map{Buffers.bufferSize(_,0)}, toChannelOpen)
  }

  override def bytesAsInts : LongTraversable[Int] = readableByteChannel.bytesAsInts
  override def bytes : LongTraversable[Byte] = readableByteChannel.bytes
}
