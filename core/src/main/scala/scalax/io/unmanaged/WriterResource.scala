package scalax.io
package unmanaged

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] (
    resource: A,
    resourceContext:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends WriteChars with Resource[A] {
  final val context = unmanagedContext(resourceContext)
  override def open:OpenedResource[A] = new UnmanagedOpenedResource(resource,unmanagedContext(context)){
    override def closeAction[U >: A] = CloseAction(_ => resource.flush())
  }
  override def updateContext(newContext:ResourceContext) = new WriterResource(resource, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) =
    new WriterResource(resource, context, newCloseAction :+ closeAction)

  
  override def writer = this
}
