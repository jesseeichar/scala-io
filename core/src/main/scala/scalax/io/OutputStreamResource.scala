package scalax.io

import java.io.{OutputStream, BufferedOutputStream, Writer, OutputStreamWriter}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.WritableChannelAdapter

/**
 * A ManagedResource for accessing and using OutputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class OutputStreamResource[+A <: OutputStream] (
    opener: => A,
    closeAction:CloseAction[A])
  extends OutputResource[A]
  with ResourceOps[A, OutputStreamResource[A]] {

  def open(): OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new OutputStreamResource[A](opener, closeAction) {
    private[this] val resource = opener
    override def open = new UnmanagedOpenedResource(resource, closeAction)
  }
  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new OutputStreamResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new OutputStreamResource(opener,closeAction +: newAction)

  def outputStream = this
  def underlyingOutput = this
  def writer(implicit sourceCodec: Codec): WriterResource[Writer] = {
    def nResource = {
      val a = open()
      new OutputStreamWriter(a.get) with Adapter[A] {
        def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromWriter(nResource).appendCloseAction(closer)
  }
  def writableByteChannel = {
    val nResource = new WritableChannelAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromWritableByteChannel(nResource).appendCloseAction(closer)
  }
}