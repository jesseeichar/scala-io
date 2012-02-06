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
    val context:ResourceContext[A])
  extends OutputResource[A]
  with ResourceOps[A, OutputResource[A], OutputStreamResource[A]] {

  self => 

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,context)
  def unmanaged = new scalax.io.unmanaged.OutputStreamResource[A](opener, context)

  def outputStream = this
  def underlyingOutput = this
  def writer(implicit sourceCodec: Codec):WriterResource[Writer] = {
    def nResource = {
      val a = open()
      new OutputStreamWriter(a.get) with Adapter[A] {
        def src = a.get
      }
    }
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WriterResource(nResource, newContext)
  }
  def writableByteChannel = {
    val nResource = new WritableChannelAdapter(opener)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WritableByteChannelResource(nResource, newContext)
  }
}