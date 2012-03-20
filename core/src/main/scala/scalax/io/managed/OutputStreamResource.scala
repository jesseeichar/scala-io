package scalax.io
package managed

import java.io.{OutputStream, BufferedOutputStream, Writer, OutputStreamWriter}
import java.nio.channels.{Channels,WritableByteChannel}
import scalax.io.ResourceAdapting.WritableChannelAdapter

/**
 * A ManagedResource for accessing and using OutputStreams.  Class can be created using the [[scalax.io.Resource]] object.
 */
class OutputStreamResource[+A <: OutputStream] (
    opener: => A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends OutputResource[A]
  with ResourceOps[A, OutputStreamResource[A]] {

  self => 

  override def open():OpenedResource[A] = new CloseableOpenedResource(opener,context, closeAction)
  override def updateContext(newContext:ResourceContext) = new OutputStreamResource(opener, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new OutputStreamResource(opener, context, newCloseAction :+ closeAction)

  override def outputStream = this
  protected override def underlyingOutput = writableByteChannel
  override def writer(implicit sourceCodec: Codec):WriterResource[Writer] = {
    def nResource = {
      val a = open()
      new OutputStreamWriter(a.get, sourceCodec.name) with Adapter[A] {
        override def src = a.get
      }
    }
    val closer = ResourceAdapting.closeAction(closeAction)
    new WriterResource(nResource, context, closer)
  }
  override def writableByteChannel = {
    def nResource = new WritableChannelAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    new WritableByteChannelResource(nResource, context, closer)
  }
}