package scalax.io

import java.io.{InputStreamReader, Reader, BufferedInputStream, InputStream}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.ReadableChannelAdapter

/**
 * A ManagedResource for accessing and using InputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class InputStreamResource[+A <: InputStream] protected[io](opener: => A,closeAction:CloseAction[A]) extends BufferableInputResource[A, BufferedInputStream]
    with ResourceOps[A, InputStreamResource[A]] {
  def open() = opener

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new InputStreamResource[A](opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new InputStreamResource[A](opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def inputStream = this
  def buffered:InputStreamResource[BufferedInputStream] = {
    def nResource = {
      val a = open()
      new BufferedInputStream(a) with ResourceAdapting.Adapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromBufferedInputStream(nResource).appendCloseAction(closer)
  }
  def reader(implicit sourceCodec: Codec): ReaderResource[Reader] = {
    def nResource = {
      val a = open()
      new InputStreamReader(a) with ResourceAdapting.Adapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromReader(nResource).appendCloseAction(closer)
  }

  def readableByteChannel = {
    val nResource = new ReadableChannelAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    Resource.fromReadableByteChannel(nResource).appendCloseAction(closer)
  }
  def chars(implicit codec: Codec) = reader(codec).chars

  def bytesAsInts : ResourceView[Int] = ResourceTraversable.streamBased(this).view
}
