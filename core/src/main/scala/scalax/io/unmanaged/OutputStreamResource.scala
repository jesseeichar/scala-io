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
    closeAction:CloseAction[A]=CloseAction.Noop)
  extends OutputResource[A]
  with ResourceOps[A, OutputStreamResource[A]]
  with UnmanagedResource {
    self => 
  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource)
  override def close() = new CloseableOpenedResource(open.get, closeAction).close()
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
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, closer)
  }
  def writableByteChannel = {
    val nResource = new WritableChannelAdapter(resource)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WritableByteChannelResource(nResource, closer)
  }
}