package scalax.io

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] (
    opener: => A,
    closeAction:CloseAction[A] = CloseAction.Noop)
  extends WriteCharsResource[A]
  with ResourceOps[A, WriterResource[A]]  {

  self =>
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new WriterResource[A](opener, CloseAction.Noop) with UnmanagedResource {
    private[this] val resource = self.open
    override def open = new UnmanagedOpenedResource(resource.get)
    def close() = resource.close()
  }

  protected def writer = this
}
