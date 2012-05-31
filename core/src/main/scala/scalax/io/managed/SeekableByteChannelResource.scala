package scalax.io
package managed

import scalax.io.ResourceAdapting.{ChannelInputStreamAdapter, ChannelWriterAdapter, ChannelReaderAdapter, ChannelOutputStreamAdapter}
import java.io.{OutputStream, Reader, Writer, InputStream}
import java.nio.channels.{ReadableByteChannel, WritableByteChannel}
import StandardOpenOption._

/**
 * A ManagedResource for accessing and using SeekableByteChannels.  Class can be created using the [[scalax.io.Resource]] object.
 */
class SeekableByteChannelResource[+A <: SeekableByteChannel] (
    opener: (Seq[OpenOption])=> A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop,
    protected val sizeFunc:() => Option[Long] = () => None,
    protected val openOptions:Option[Seq[OpenOption]] = None)
  extends SeekableResource[A]
  with ResourceOps[A, SeekableByteChannelResource[A]]  {


  self =>

  private def rawOpen() = opener(openOptions getOrElse ReadWrite)
  def writeAppendOpen = opener(openOptions getOrElse WriteAppend)

  override def open():OpenedResource[A] = new CloseableOpenedResource(rawOpen(),context, closeAction)
  override def updateContext(newContext:ResourceContext) =
    new SeekableByteChannelResource(opener, newContext, closeAction, sizeFunc, openOptions)
  override def addCloseAction(newCloseAction: CloseAction[A]) =
    new SeekableByteChannelResource(opener, context, newCloseAction :+ closeAction, sizeFunc, openOptions)

  override def inputStream = {
    def nResource = new ChannelInputStreamAdapter(rawOpen())
    val closer = ResourceAdapting.closeAction(closeAction)
    new InputStreamResource(nResource,context, closer,sizeFunc)
  }
  override def outputStream = {
    def nResource = new ChannelOutputStreamAdapter(writeAppendOpen)
    val closer = ResourceAdapting.closeAction(closeAction)
    new OutputStreamResource(nResource, context, closer)
  }
  override def reader(implicit sourceCodec: Codec) = {
    def nResource = new ChannelReaderAdapter(rawOpen(),sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    new ReaderResource(nResource, context, closer)
  }
  override def writer(implicit sourceCodec: Codec) = {
    def nResource = new ChannelWriterAdapter(writeAppendOpen,sourceCodec)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }

  def writableByteChannel = new WritableByteChannelResource(writeAppendOpen, context, closeAction)
  def readableByteChannel = new ReadableByteChannelResource(rawOpen(),context, closeAction,sizeFunc)

  protected override def underlyingChannel(append:Boolean) = {
    val resource:A = append match {
      case true =>
        val c = opener(ReadWrite :+ Append)
        val pos = c.position
        val size = c.size
        if(pos < size) {
          c.position(size)
        }
        c
      case false =>
        opener(ReadWrite)
    }

    new CloseableOpenedResource(resource,context, closeAction)
  }

  protected override def underlyingOutput: OutputResource[WritableByteChannel] = this
  override def blocks(blockSize: Option[Int] = None):LongTraversable[ByteBlock] =
    new traversable.ChannelBlockLongTraversable(blockSize, context, sizeFunc, open)

  override def bytesAsInts:LongTraversable[Int] = ResourceTraversable.seekableByteChannelBased[Byte,Int](this.open, context, sizeFunc, initialConv = ResourceTraversable.toIntConv)
  override def bytes:LongTraversable[Byte] = ResourceTraversable.seekableByteChannelBased[Byte,Byte](this.open, context, sizeFunc)

  override def toString: String = "SeekableByteChannelResource("+context.descName.name+")"
}
