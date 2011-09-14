package scalax.io

import java.io.{InputStreamReader, Reader, BufferedInputStream, InputStream}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.ReadableChannelAdapter
import java.lang.String
import java.nio.ByteBuffer

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
      new InputStreamReader(a.get) with ResourceAdapting.Adapter[A] {
        def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource,closer,descName)

  }

  def readableByteChannel = {
    val nResource = new ReadableChannelAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromReadableByteChannel(nResource).appendCloseAction(closer)
  }
  def chars(implicit codec: Codec) = reader(codec).chars

  def bytesAsInts : ResourceView[Int] = ResourceTraversable.streamBased[Byte,Int](this.open, sizeFunc,initialConv = ResourceTraversable.toIntConv).view
  override def bytes : ResourceView[Byte] = ResourceTraversable.streamBased[Byte,Byte](this.open, sizeFunc).view
}
