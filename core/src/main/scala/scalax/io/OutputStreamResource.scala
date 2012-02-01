package scalax.io

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
  with ResourceOps[A, OutputStreamResource[A]] {
    self => 
  def open(): OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new OutputStreamResource[A](opener, CloseAction.Noop) with UnmanagedResource {
    private[this] val resource = self.open
    override def open = new UnmanagedOpenedResource(resource.get)
    def close() = resource.close()
  }

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
    if(isManaged) new WriterResource(nResource, closer)
    else new WriterResource(nResource, closer) with UnmanagedResourceAdapter
  }
  def writableByteChannel = {
    val nResource = new WritableChannelAdapter(opener)
    val closer = ResourceAdapting.closeAction(closeAction)
    if(isManaged) new WritableByteChannelResource(nResource, closer)
    else new WritableByteChannelResource(nResource, closer) with UnmanagedResourceAdapter
  }
}