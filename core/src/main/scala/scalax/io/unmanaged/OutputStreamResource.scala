package scalax.io
package unmanaged

import java.io.{OutputStream, BufferedOutputStream, Writer, OutputStreamWriter}
import java.nio.channels.Channels
import scalax.io.ResourceAdapting.WritableChannelAdapter
import java.nio.channels.WritableByteChannel

/**
 * A ManagedResource for accessing and using OutputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class OutputStreamResource[+A <: OutputStream] (
    resource: A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends OutputResource[A]
  with ResourceOps[A, OutputResource[A], OutputStreamResource[A]]
  with UnmanagedResource {

  self => 
  type Repr = OutputStreamResource[A]

  override final val open:OpenedResource[A] = new UnmanagedOpenedResource(resource, unmanagedContext(context))
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override def updateContext(newContext:ResourceContext) = 
    new OutputStreamResource(resource, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new OutputStreamResource(resource, context, newCloseAction :+ closeAction)
  override final val unmanaged = this

  override def outputStream = this
  protected override def underlyingOutput = this
  override def writer(implicit sourceCodec: Codec):WriterResource[Writer] = {
    def nResource = {
      val a = open
      new OutputStreamWriter(a.get) with Adapter[A] {
        override def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }
  override def writableByteChannel = {
    def nResource = new WritableChannelAdapter(resource)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WritableByteChannelResource(nResource, context, closer)
  }
}