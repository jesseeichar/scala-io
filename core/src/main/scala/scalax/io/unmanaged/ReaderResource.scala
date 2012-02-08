package scalax.io
package unmanaged

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    resource: A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends ReadCharsResource[A]
  with ResourceOps[A, ReadCharsResource[A], ReaderResource[A]]
  with UnmanagedResource {

  self => 

  override final val open:OpenedResource[A] = new UnmanagedOpenedResource(resource, unmanagedContext(context))
  override def close() = new CloseableOpenedResource(open.get, context, closeAction).close()
  override def updateContext(newContext:ResourceContext) = 
    new ReaderResource(resource, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) = 
    new ReaderResource(resource, context, newCloseAction :+ closeAction)
  override final val unmanaged = this

  override def chars : LongTraversable[Char]= ResourceTraversable.readerBased(this.open)

  override def toString: String = "ReaderResource("+context.descName.name+")"
}
