package scalax.io
package managed

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] (
    opener: => A,
    closeAction:CloseAction[A] = CloseAction.Noop)
  extends WriteCharsResource[A]
  with ResourceOps[A, WriteCharsResource[A]]  {

  self =>
  def open():OpenedResource[A] = new CloseableOpenedResource(opener,closeAction)
  def unmanaged = new scalax.io.unmanaged.WriterResource[A](opener, CloseAction.Noop)

  protected def writer = this
}
