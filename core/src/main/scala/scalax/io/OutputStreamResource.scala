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
  extends BufferableOutputResource[A, BufferedOutputStream]
  with ResourceOps[A, OutputStreamResource[A]] {

  def open() = opener
  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new OutputStreamResource(opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new OutputStreamResource(opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def outputStream = this
  def underlyingOutput = this
  def buffered:OutputStreamResource[BufferedOutputStream] = {
    def nResource = {
      val a = open()
      new BufferedOutputStream(a) with ResourceAdapting.Adapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromBufferedOutputStream(nResource).appendCloseAction(closer)
  }
  def writer(implicit sourceCodec: Codec): WriterResource[Writer] = {
    def nResource = {
      val a = open()
      new OutputStreamWriter(a) with ResourceAdapting.Adapter[A] {
        def src = a
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