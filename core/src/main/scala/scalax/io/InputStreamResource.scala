package scalax.io

import java.io.{InputStreamReader, Reader, BufferedInputStream, InputStream}
import java.nio.channels.Channels

/**
 * A ManagedResource for accessing and using InputStreams.
 *
 * @see ManagedResource
 */
class InputStreamResource[+A <: InputStream](opener: => A,closeAction:CloseAction[A]) extends BufferableInputResource[A, BufferedInputStream]
    with ResourceOps[A, InputStreamResource[A]] {
  def open() = opener

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new InputStreamResource[A](opener,newAction :+ closeAction)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new InputStreamResource[A](opener,closeAction +: newAction)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def inputStream = this
  def buffered:InputStreamResource[BufferedInputStream] = {
    def nResource = {
      val a = open()
      new BufferedInputStream(a) with ResourceAdapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromBufferedInputStream(nResource)(closer)
  }
  def reader(implicit sourceCodec: Codec): ReaderResource[Reader] = {
    def nResource = {
      val a = open()
      new InputStreamReader(a) with ResourceAdapter[A] {
        def src = a
      }
    }
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromReader(nResource)(closer)
  }

  def readableByteChannel = {
    val nResource = new ReadableChannelAdapter(opener)
    val closer = ResourceAdapter.closeAction(closeAction)
    Resource.fromReadableByteChannel(nResource)(closer)
  }
  def chars(implicit codec: Codec) = reader(codec).chars

  def bytesAsInts : ResourceView[Int] = ResourceTraversable.streamBased(this).view
}
