package scalax.io
package unmanaged

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] (
    resource: A,
    val context: ResourceContext[A])
  extends WriteCharsResource[A]
  with ResourceOps[A, WriteCharsResource[A], WriterResource[A]]
  with UnmanagedResource {

  self =>
  override def open():OpenedResource[A] = new UnmanagedOpenedResource(resource, context)
  override def close() = new CloseableOpenedResource(open.get, context).close()
  override def unmanaged = this

  protected def writer = this
}
