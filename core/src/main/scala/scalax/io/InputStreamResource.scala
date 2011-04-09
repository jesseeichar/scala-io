package scalax.io

import java.io.{InputStreamReader, Reader, BufferedInputStream, InputStream}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.ReadableChannelAdapter

/**
 * A ManagedResource for accessing and using InputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class InputStreamResource[+A <: InputStream] (
    opener: => A,
    closeAction:CloseAction[A],
    protected val sizeFunc:() => Option[Long] )
  extends InputResource[A]
  with ResourceOps[A, InputStreamResource[A]] {

  def open() = opener

  def prependCloseAction[B >: A](newAction: CloseAction[B]) = new InputStreamResource[A](opener,newAction :+ closeAction,sizeFunc)
  def appendCloseAction[B >: A](newAction: CloseAction[B]) = new InputStreamResource[A](opener,closeAction +: newAction,sizeFunc)

  override def acquireFor[B](f: (A) => B) = new CloseableResourceAcquirer(open,f,closeAction)()

  def inputStream = this
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
