package scalax.io

import scalax.io.ResourceAdapting.{ChannelInputStreamAdapter, ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter}
import java.io.OutputStream
import StandardOpenOption._

/**
 * A ManagedResource for accessing and using SeekableByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class SeekableByteChannelResource[+A <: SeekableByteChannel] (
    opener: (Seq[OpenOption])=> A,
    closeAction:CloseAction[A],
    protected val sizeFunc:() => Option[Long],
    descName:ResourceDescName,
    protected val openOptions:Option[Seq[OpenOption]])
  extends SeekableResource[A]
  with ResourceOps[A, SeekableByteChannelResource[A]]  {
  private def rawOpen() = opener(openOptions getOrElse ReadWrite)
  def open():OpenedResource[A] = new CloseableOpenedResource(rawOpen(),closeAction)

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new SeekableByteChannelResource(opener,newAction :+ closeAction,sizeFunc,descName,openOptions)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new SeekableByteChannelResource(opener,closeAction +: newAction,sizeFunc,descName,openOptions)

  def inputStream = {
    def nResource = new ChannelInputStreamAdapter(rawOpen())
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,closer,sizeFunc,descName)
  }
  def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(rawOpen())
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromOutputStream(nResource).appendCloseAction(closer)
  }
  def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(rawOpen(),sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromReader(nResource).appendCloseAction(closer)
  }
  def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(rawOpen(),sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromWriter(nResource).appendCloseAction(closer)
  }
  def writableByteChannel = Resource.fromWritableByteChannel(rawOpen()).appendCloseAction(closeAction)
  def readableByteChannel = new ReadableByteChannelResource(rawOpen(),closeAction,sizeFunc,descName)
  def byteChannel = new ByteChannelResource(rawOpen(),closeAction,sizeFunc)

  override def bytesAsInts = inputStream.bytesAsInts // TODO optimize for byteChannel
  override def chars(implicit codec: Codec) = reader(codec).chars  // TODO optimize for byteChannel


  protected override def underlyingChannel(append:Boolean) = {
    val resource:A = (openOptions,append) match {
      case (None,true) =>
        val c = opener(ReadWrite)
        c.position(c.size)
        c
      case (Some(_),true) =>
        opener(ReadWrite :+ Append)
      case (_,false) =>
        opener(ReadWrite)
    }

    new CloseableOpenedResource(resource,closeAction)
  }

  protected override def underlyingOutput: OutputResource[OutputStream] = outputStream

  override def toString: String = "SeekableByteChannelResource("+descName.name+")"
}

