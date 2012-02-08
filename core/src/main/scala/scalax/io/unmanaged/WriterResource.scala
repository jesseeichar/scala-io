package scalax.io
package unmanaged

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] (
    resource: A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends WriteCharsResource[A]
  with ResourceOps[A, WriteCharsResource[A], WriterResource[A]]
  with UnmanagedResource {

  self =>
  override final val open:OpenedResource[A] = new UnmanagedOpenedResource(resource,unmanagedContext(context)){
    override def closeAction[U >: A] = CloseAction(_ => resource.flush())
  }
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override def newContext(newContext:ResourceContext) = 
    new WriterResource(resource, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new WriterResource(resource, context, newCloseAction :+ closeAction)
  override final val unmanaged = this

  protected def writer = this
}
