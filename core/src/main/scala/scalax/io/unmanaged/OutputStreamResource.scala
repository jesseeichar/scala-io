package scalax.io
package unmanaged

import java.io.{OutputStream, BufferedOutputStream, Writer, OutputStreamWriter}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.WritableChannelAdapter

/**
 * A ManagedResource for accessing and using OutputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class OutputStreamResource[+A <: OutputStream] (
    resource: A,
    val context: ResourceContext[A])
  extends OutputResource[A]
  with ResourceOps[A, OutputResource[A], OutputStreamResource[A]]
  with UnmanagedResource {

  self => 
  type Repr = OutputStreamResource[A]

  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource, context)
  override def close() = new CloseableOpenedResource(open.get, context).close()
  override def unmanaged = this

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
    val nResource = new WritableChannelAdapter(resource)
    val newContext = context.copy(closeAction = ResourceAdapting.closeAction(context.closeAction))
    new WritableByteChannelResource(nResource, newContext)
  }
}