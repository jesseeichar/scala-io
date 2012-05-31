package scalax.io
package managed

import java.io.{Reader, BufferedReader}

/**
 * A ManagedResource for accessing and using Readers.  Class can be created using the [[scalax.io.Resource]] object.
 */
class ReaderResource[+A <: Reader] (
    opener: => A,
    val context:ResourceContext = DefaultResourceContext,
    closeAction: CloseAction[A] = CloseAction.Noop)
  extends ReadCharsResource[A]
  with ResourceOps[A, ReaderResource[A]] {

  self =>

  override def open():OpenedResource[A] = new CloseableOpenedResource(opener,context, closeAction)
  override def updateContext(newContext:ResourceContext) =
    new ReaderResource(opener, newContext, closeAction)
  override def addCloseAction(newCloseAction: CloseAction[A]) =
    new ReaderResource(opener, context, newCloseAction :+ closeAction)

  override def chars : LongTraversable[Char]=
    ResourceTraversable.readerBased(this.open, context)

  override def toString: String = "ReaderResource("+context.descName.name+")"
}
