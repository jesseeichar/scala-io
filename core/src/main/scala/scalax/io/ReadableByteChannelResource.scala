package scalax.io

import java.io.BufferedInputStream
import java.nio.channels.{Channels, ReadableByteChannel}
import scalax.io.ResourceAdapting.{ChannelReaderAdapter, ChannelInputStreamAdapter}

/**
 * A ManagedResource for accessing and using ByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReadableByteChannelResource[+A <: ReadableByteChannel] (
    opener: => A,
    closeAction:CloseAction[A],
    protected val sizeFunc:() => Option[Long],
    descName:ResourceDescName)
  extends InputResource[A]
  with ResourceOps[A, ReadableByteChannelResource[A]] {

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new ReadableByteChannelResource(opener,newAction :+ closeAction,sizeFunc,descName)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new ReadableByteChannelResource(opener,closeAction +: newAction,sizeFunc,descName)

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource, closer, sizeFunc,descName)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(opener,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromReader(nResource).appendCloseAction(closer)
  }
  def readableByteChannel = this
  override def bytesAsInts = ResourceTraversable.byteChannelBased[Byte,Int](this.open, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes = ResourceTraversable.byteChannelBased[Byte,Byte](this.open, sizeFunc)
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel

  override def toString: String = "ReadableByteChannelResource ("+descName.name+")"
}
