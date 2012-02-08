package scalax.io
package managed

import java.io.{Writer, BufferedWriter}
/**
 * A ManagedResource for accessing and using Writers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class WriterResource[+A <: Writer] (
    opener: => A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends WriteCharsResource[A]
  with ResourceOps[A, WriteCharsResource[A], WriterResource[A]]  {

  self => 

  override def open():OpenedResource[A] = new CloseableOpenedResource(opener,context, closeAction)
  override def unmanaged = new scalax.io.unmanaged.WriterResource[A](opener, context, closeAction)
  override def newContext(newContext:ResourceContext) = 
    new WriterResource(opener, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new WriterResource(opener, context, newCloseAction :+ closeAction)

  protected def writer = this
}
