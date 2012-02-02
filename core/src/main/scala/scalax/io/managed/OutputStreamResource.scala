package scalax.io
package managed

import java.io.{OutputStream, BufferedOutputStream, Writer, OutputStreamWriter}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.WritableChannelAdapter

/**
 * A ManagedResource for accessing and using OutputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class OutputStreamResource[+A <: OutputStream] (
    opener: => A,
    closeAction:CloseAction[A]=CloseAction.Noop)
  extends OutputResource[A]
  with ResourceOps[A, OutputResource[A]] {
    self => 
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new scalax.io.unmanaged.OutputStreamResource[A](opener, CloseAction.Noop)

  def outputStream = this
  def underlyingOutput = this
  def writer(implicit sourceCodec: Codec):WriterResource[Writer] = {
    def nResource = {
      val a = open()
      new OutputStreamWriter(a.get) with Adapter[A] {
        def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, closer)
  }
  def writableByteChannel = {
    val nResource = new WritableChannelAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WritableByteChannelResource(nResource, closer)
  }
}