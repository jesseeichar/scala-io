package scalax.io
package managed

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] (
    opener: => A,
    val context:ResourceContext[A])
  extends WriteCharsResource[A]
  with ResourceOps[A, WriteCharsResource[A], WriterResource[A]]  {

  self => 

  def open():OpenedResource[A] = new CloseableOpenedResource(opener,context)
  def unmanaged = new scalax.io.unmanaged.WriterResource[A](opener, context)

  protected def writer = this
}
